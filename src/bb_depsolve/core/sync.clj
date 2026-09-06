(ns bb-depsolve.core.sync
  "The sync command: discover, compute and apply internal pin changes."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [bb-depsolve.version.api :as v]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.schema.api :as sch]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.core.resolve :as resolve]
            [hive-weave.parallel :as par]
            [bb-depsolve.core.resolve.registries :as registries]
            [bb-depsolve.version.repos :as repos]
            [bb-depsolve.core.parity :as parity]
            [bb-depsolve.graph.collect :as collect]))

(defn discover-internal-libs
  "Auto-discover internal deps by scanning dep files for io.github.{org}/* coords,
   in both :git/tag+:git/sha and :mvn/version form.
   Returns map of lib-sym -> {:dir-name string :coords #{:git|:mvn}}.
   The coordinate kinds a lib is actually pinned by decide what has to be
   resolved for it: tags for :git, published registry versions for :mvn."
  [dep-files org]
  (->> dep-files
       (remove discovery/shadow-deps-file?)
       (mapcat (fn [{:keys [path]}]
                 (let [content (slurp path)]
                   (concat (map #(assoc % :coord :git) (v/find-git-deps content))
                           (map #(assoc % :coord :mvn) (v/find-mvn-deps content))))))
       (filter #(v/lib-matches-org? org (:lib %)))
       (reduce (fn [libs {:keys [lib coord]}]
                 (-> libs
                     (assoc-in [lib :dir-name] (v/lib-artifact-id lib))
                     (update-in [lib :coords] (fnil conj #{}) coord)))
               {})))

(defn resolve-sync-lib
  "Resolve only the coordinate kinds actually used for an internal lib.
   Git coords come from tags; Maven coords from the registries, read PER
   REGISTRY (:mvn-by-registry) so each consumer can be pinned to what it can
   reach, with the registries that did not answer kept in :mvn-unread so a
   blind read holds a pin instead of moving it. :mvn-version is the
   resolver-wide newest. Returns Result<resolved-lib>, partial when only one
   coordinate kind resolves; :io/registry-unread when nothing answered but
   some registry failed to; :io/no-resolved-coordinate when nothing lists it."
  [root-dir lib-sym {:keys [dir-name coords]}]
  (let [tag-result (when (contains? coords :git)
                     (resolve/resolve-lib-tags root-dir lib-sym dir-name))
        {:keys [versions unread]} (when (contains? coords :mvn)
                                    (registries/resolve-mvn-reads lib-sym false))
        tag-info (when (r/ok? tag-result) (:ok tag-result))
        resolved (cond-> (or tag-info {})
                   (seq versions) (assoc :mvn-version (:version (repos/newest versions))
                                         :mvn-by-registry (vec (sort-by :id versions)))
                   (seq unread) (assoc :mvn-unread (vec unread)))]
    (cond
      (or tag-info (seq versions))
      (r/ok (sch/validate! :bb-depsolve/resolved-lib resolved))

      (seq unread)
      (r/err :io/registry-unread {:lib lib-sym :unread unread})

      :else
      (r/err :io/no-resolved-coordinate {:lib lib-sym :coords coords}))))

(def ^:private resolve-concurrency
  "Simultaneous tag lookups. Each is a `git ls-remote` (or a registry call), so
   the ceiling is remote politeness, not local CPU."
  8)

(def ^:private resolve-timeout-ms 30000)

(defn resolve-internal-libs
  "Resolve every internal lib's coordinates with bounded fan-out.
   Returns {:resolved {lib-sym resolved-lib} :failed [{:lib :error}]}.

   Resolution is one or more remote round-trips per lib and dominates sync's
   wall clock, so the lookups run concurrently. A lib that times out or throws
   surfaces in :failed as :io/resolve-failed rather than vanishing from the
   report."
  [root-dir internal-libs]
  (let [entries (vec internal-libs)
        results (par/bounded-pmap
                 {:concurrency resolve-concurrency
                  :timeout-ms  resolve-timeout-ms
                  :fallback    nil}
                 (fn [[lib-sym discovery]]
                   (resolve-sync-lib root-dir lib-sym discovery))
                 entries)]
    (reduce (fn [acc [[lib-sym _] result]]
              (if (and result (r/ok? result))
                (assoc-in acc [:resolved lib-sym] (:ok result))
                (update acc :failed conj
                        {:lib lib-sym
                         :error (if result (:error result) :io/resolve-failed)})))
            {:resolved {} :failed []}
            (map vector entries results))))

(defn resolution-line
  "One report line for a resolved lib. A lib pinned only by :mvn/version has no
   tag and no tag source, so every part is optional. When the Maven version
   was resolved per registry, each registry's newest is listed."
  [{:keys [tag sha-short sha source mvn-version mvn-by-registry]}]
  (str/join "  "
            (cond-> []
              tag         (conj (str (ui/c :green tag) " -> " (ui/c :dim (or sha-short sha))))
              mvn-version (conj (str (ui/c :green mvn-version)
                                     (ui/c :dim (str " (mvn"
                                                     (when (seq mvn-by-registry)
                                                       (str ": "
                                                            (str/join ", " (map #(str (:id %) " " (:version %))
                                                                                mvn-by-registry))))
                                                     ")"))))
              source      (conj (ui/c :dim (str "(" (name source) ")"))))))

(defn compute-sync-changes
  "Compute sync changes between dep files and resolved lib versions.
   Covers both git coords (:git/tag+:git/sha) and maven coords (:mvn/version).

   RESOLVED is projected through each dep file's own `:mvn/repos` before the
   comparison, so a project is only ever moved to a version the registries it
   declares can serve. Pure calculation delegated to
   bb-depsolve.version.api/sync-changes-in-content. Each change map carries
   :coord (:git or :mvn) plus :path/:project. Output is schema-validated at
   this boundary (fail-loud)."
  [dep-files resolved]
  (->> dep-files
       (remove discovery/shadow-deps-file?)
       (mapcat (fn [{:keys [path project]}]
                 (let [content (slurp path)
                       projected (repos/project-resolved resolved (repos/declared-repos content))]
                   (->> (v/sync-changes-in-content content projected)
                        (map #(assoc % :path path :project project))))))
       (vec)
       (sch/validate! :bb-depsolve/sync-changes)))

(defn compute-withheld
  "Maven pins in DEP-FILES that no registry the pinning project declares can
   satisfy at ANY published version. Such a pin is left alone by sync and
   reported instead. Returns [{:project :path :lib :versions} ...]."
  [dep-files resolved]
  (->> dep-files
       (remove discovery/shadow-deps-file?)
       (mapcat (fn [{:keys [path project]}]
                 (let [content (slurp path)]
                   (map #(assoc % :path path :project project)
                        (repos/withheld resolved
                                        (repos/declared-repos content)
                                        (map :lib (v/find-mvn-deps content)))))))
       (vec)))

(defn apply-sync-changes!
  "Apply sync changes to files, dispatching on :coord.
   :git entries update :git/tag+:git/sha; :mvn entries update :mvn/version.
   Action: writes to disk."
  [root-dir changes]
  (let [by-file (group-by :path changes)]
    (doseq [[path file-changes] by-file
            :let [content (atom (slurp path))]]
      (doseq [{:keys [coord lib new-tag new-sha new-version]} file-changes]
        (if (= coord :mvn)
          (swap! content v/update-mvn-dep lib new-version)
          (swap! content v/update-git-dep lib new-tag new-sha)))
      (spit path @content)
      (println (ui/c :green (str "  Updated " (str (fs/relativize root-dir path))))))
    (println)
    (println (ui/c :green (format "Applied %d changes." (count changes))))))

(defn- workspace
  "Root, dep files and internal libs for OPTS. Exits when --org is missing."
  [{:keys [root org skip-dirs depth]
    :or {root "." depth discovery/default-depth}}]
  (let [root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        dep-files (discovery/find-dep-files {:root root :skip-dirs skip-set :depth depth})]
    (when-not org
      (println (ui/c :red "Error: --org is required (e.g. --org hive-agi)"))
      (System/exit 1))
    {:root-dir root-dir
     :skip-set skip-set
     :dep-files dep-files
     :org org
     :internal-libs (discover-internal-libs dep-files org)}))

(defn- print-resolution!
  "One line per resolved lib, one per failure. Action: prints."
  [resolved failed]
  (doseq [[lib-sym info] (sort-by (comp str key) resolved)]
    (printf "  %-40s %s\n" (ui/c :cyan (str lib-sym)) (resolution-line info)))
  (doseq [{:keys [lib error]} (sort-by (comp str :lib) failed)]
    (printf "  %-40s %s\n"
            (ui/c :cyan (str lib))
            (ui/c :yellow (str "unresolved (" error ")"))))
  (println))

(defn- print-degraded!
  "Warn when the resolution was a degraded read: a registry did not answer
   for some lib, so its pins are held and the plan is partial. Action: prints."
  [resolved failed]
  (let [unread-libs (count (filter (comp seq :mvn-unread val) resolved))
        blind (count (filter #(contains? #{:io/registry-unread :io/resolve-failed} (:error %)) failed))]
    (when (pos? (+ unread-libs blind))
      (println (ui/c :yellow (format "Degraded read: %d lib(s) had a registry that did not answer, %d could not be resolved at all. Their pins are held; re-run before trusting this plan."
                                     unread-libs blind)))
      (println))))

(defn- print-parity!
  "Registry-parity findings for RESOLVED, when any. Action: prints."
  [root-dir resolved]
  (let [findings (parity/findings root-dir resolved)]
    (when (seq findings)
      (parity/print-findings! root-dir findings)
      (println))))

(defn- read-failure-label
  "How a registry that did not answer is named in a report."
  [{:keys [id status message]}]
  (str id " (" (or (some->> status (str "HTTP ")) message "no response") ")"))

(defn- print-withheld!
  "Pins sync will not move, with the reason, when any. Action: prints."
  [withheld]
  (when (seq withheld)
    (println (ui/c :yellow (format "%d pin(s) held back:" (count withheld))))
    (doseq [{:keys [project lib reason versions unread path]} withheld]
      (printf "  %-25s %-35s %s\n"
              (ui/c :cyan project) (str lib)
              (case reason
                :unread
                (ui/c :red (str "did not answer: "
                                (str/join ", " (map read-failure-label unread))
                                "; a blind read moves no pin"))
                :unreachable
                (ui/c :dim (str "only on "
                                (str/join ", " (map #(str (:id %) " " (:version %)) versions))
                                "; declare it under :mvn/repos in " (fs/file-name path)
                                " or pin a public release")))))
    (println)))

(defn- unreachable-note
  "Trailing note for a change whose lib has newer versions the project cannot
   fetch; empty when there are none."
  [unreachable]
  (if (seq unreachable)
    (ui/c :yellow (str "  "
                       (str/join ", " (map #(str (:id %) " has " (:version %)) unreachable))
                       ", not declared by this project"))
    ""))

(defn- print-changes!
  "The mismatch table, and a warning for rows that move a pin DOWN. Action:
   prints."
  [changes]
  (println (ui/c :yellow (format "%d mismatches found:" (count changes))))
  (println)
  (doseq [{:keys [coord project lib old-tag old-sha new-tag new-sha
                  old-version new-version source unreachable]} changes]
    (if (= coord :mvn)
      (printf "  %-25s %-35s %s -> %s  (mvn%s)%s\n"
              (ui/c :cyan project) (str lib)
              (ui/c :red old-version) (ui/c :green new-version)
              (if source (str " via " source) "")
              (unreachable-note unreachable))
      (printf "  %-25s %-35s %s %s -> %s %s\n"
              (ui/c :cyan project) (str lib)
              (ui/c :red old-tag) (ui/c :dim old-sha)
              (ui/c :green new-tag) (ui/c :dim new-sha))))
  (println)
  (let [downgrades (count (filter v/downgrade-change? changes))]
    (when (pos? downgrades)
      (println (ui/c :yellow (format "%d row(s) move a pin DOWN. A registry answering stale or partial data looks exactly like this; --apply refuses them unless --allow-downgrade."
                                     downgrades)))
      (println))))

(defn sync-cmd
  "Sync internal deps (git tag+sha and maven version coords) across all
   workspace projects. Every Maven pin is chosen from the registries the
   CONSUMING project declares, so a public project is never raised to a
   version only the private registry holds; that divergence is reported as a
   registry-parity finding instead, with the forge sync that fixes it. A
   registry that did not answer holds the pins that depend on it, and
   --apply refuses a plan that moves any pin down unless --allow-downgrade."
  [{:keys [opts]}]
  (let [{:keys [apply commit allow-downgrade]} opts
        {:keys [root-dir dep-files internal-libs org]} (workspace opts)]
    (println (ui/c :bold (format "Resolving %s coordinates (%d libs)..." (str "io.github." org) (count internal-libs))))
    (println)
    (let [{:keys [resolved failed]} (resolve-internal-libs root-dir internal-libs)]
      (print-resolution! resolved failed)
      (print-degraded! resolved failed)
      (print-parity! root-dir resolved)
      (println (ui/c :bold (format "Scanning %d dep files..." (count dep-files))))
      (println)
      (print-withheld! (compute-withheld dep-files resolved))
      (let [changes (compute-sync-changes dep-files resolved)
            downgrades (count (filter v/downgrade-change? changes))]
        (if (empty? changes)
          (println (ui/c :green "All internal deps are in sync."))
          (do
            (print-changes! changes)
            (cond
              (not apply)
              (println (ui/c :dim "  Dry run. Pass --apply to write changes."))

              (and (pos? downgrades) (not allow-downgrade))
              (do (println (ui/c :red (format "Refusing --apply: %d row(s) move a pin down. Re-run when every registry answers, or pass --allow-downgrade for a deliberate rollback."
                                              downgrades)))
                  (System/exit 1))

              :else
              (do (apply-sync-changes! root-dir changes)
                  (when commit
                    (git/auto-commit-workspace! root-dir dep-files
                                            "chore: sync internal deps (bb-depsolve)"))))))))))

(defn parity-cmd
  "Report libs whose declared publish target disagrees with where their
   artifacts are: above all a public lib whose private registry is ahead of
   the public one, which no public consumer can follow. Covers every lib the
   workspace pins by :mvn/version plus every checkout declaring
   `:publish :clojars`. Read-only. Exits 1 unless --no-fail when a finding
   blocks, or when a registry did not answer (parity cannot be certified from
   a blind read)."
  [{:keys [opts]}]
  (let [{:keys [root-dir skip-set internal-libs org]} (workspace opts)
        public-nodes (into {}
                           (keep (fn [{:keys [lib project dir]}]
                                   (when (contains? parity/public-targets (collect/publish-target dir))
                                     [lib {:dir-name project :coords #{:mvn}}])))
                           (collect/collect-nodes root-dir skip-set org))
        libs (into public-nodes
                   (keep (fn [[lib {:keys [dir-name coords]}]]
                           (when (contains? coords :mvn)
                             [lib {:dir-name dir-name :coords #{:mvn}}])))
                   internal-libs)]
    (println (ui/c :bold (format "Resolving %s registry versions (%d libs)..." (str "io.github." org) (count libs))))
    (println)
    (let [{:keys [resolved failed]} (resolve-internal-libs root-dir libs)
          findings (parity/findings root-dir resolved)
          blind? (some #(contains? #{:io/registry-unread :io/resolve-failed} (:error %)) failed)]
      (print-resolution! resolved failed)
      (print-degraded! resolved failed)
      (if (empty? findings)
        (println (ui/c :green "Registries agree with every declared publish target."))
        (parity/print-findings! root-dir findings))
      (println)
      (when (and (or (parity/blocking? findings) blind?) (not (:no-fail opts)))
        (System/exit 1)))))