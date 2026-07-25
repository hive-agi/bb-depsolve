(ns bb-depsolve.core.sync
  "The sync command: discover, compute and apply internal pin changes."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [bb-depsolve.version :as v]
            [bb-depsolve.ui :as ui]
            [bb-depsolve.schema :as sch]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.core.resolve :as resolve]
            [hive-weave.parallel :as par]
            [bb-depsolve.core.resolve.registries :as registries]))

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
   Git coords come from tags; Maven coords come from published registries — a
   tag is not proof that an artifact exists. Returns Result<resolved-lib>,
   partial when only one coordinate kind resolves."
  [root-dir lib-sym {:keys [dir-name coords]}]
  (let [tag-result (when (contains? coords :git)
                     (resolve/resolve-lib-tags root-dir lib-sym dir-name))
        mvn-result (when (contains? coords :mvn)
                     (registries/resolve-mvn-latest lib-sym false))
        tag-info (when (r/ok? tag-result) (:ok tag-result))
        mvn-version (when (r/ok? mvn-result) (:ok mvn-result))
        resolved (cond-> (or tag-info {})
                   mvn-version (assoc :mvn-version mvn-version))]
    (if (seq resolved)
      (r/ok (sch/validate! :bb-depsolve/resolved-lib resolved))
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
   tag and no tag source, so every part is optional."
  [{:keys [tag sha-short sha source mvn-version]}]
  (str/join "  "
            (cond-> []
              tag         (conj (str (ui/c :green tag) " -> " (ui/c :dim (or sha-short sha))))
              mvn-version (conj (str (ui/c :green mvn-version) (ui/c :dim " (mvn)")))
              source      (conj (ui/c :dim (str "(" (name source) ")"))))))

(defn compute-sync-changes
  "Compute sync changes between dep files and resolved lib versions.
   Covers both git coords (:git/tag+:git/sha) and maven coords (:mvn/version).
   Pure calculation delegated to bb-depsolve.version/sync-changes-in-content.
   Each change map carries :coord (:git or :mvn) plus :path/:project.
   Output is schema-validated at this boundary (fail-loud)."
  [dep-files resolved]
  (->> dep-files
       (remove discovery/shadow-deps-file?)
       (mapcat (fn [{:keys [path project]}]
                 (->> (v/sync-changes-in-content (slurp path) resolved)
                      (map #(assoc % :path path :project project)))))
       (vec)
       (sch/validate! :bb-depsolve/sync-changes)))

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

(defn sync-cmd
  "Sync internal deps (git tag+sha and maven version coords) across all workspace projects."
  [{:keys [opts]}]
  (let [{:keys [root org apply commit skip-dirs depth]
         :or {root "." depth discovery/default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        dep-files (discovery/find-dep-files {:root root :skip-dirs skip-set :depth depth})]

    (when-not org
      (println (ui/c :red "Error: --org is required for sync (e.g. --org hive-agi)"))
      (System/exit 1))

    (let [internal-libs (discover-internal-libs dep-files org)]
      (println (ui/c :bold (format "Resolving %s coordinates (%d libs)..." (str "io.github." org) (count internal-libs))))
      (println)

      (let [{:keys [resolved failed]} (resolve-internal-libs root-dir internal-libs)]

        (doseq [[lib-sym info] (sort-by (comp str key) resolved)]
          (printf "  %-40s %s\n" (ui/c :cyan (str lib-sym)) (resolution-line info)))
        (doseq [{:keys [lib error]} (sort-by (comp str :lib) failed)]
          (printf "  %-40s %s\n"
                  (ui/c :cyan (str lib))
                  (ui/c :yellow (str "unresolved (" error ")"))))
        (println)

        (println (ui/c :bold (format "Scanning %d dep files..." (count dep-files))))
        (println)

        (let [changes (compute-sync-changes dep-files resolved)]
          (if (empty? changes)
            (println (ui/c :green "All internal deps are in sync."))
            (do
              (println (ui/c :yellow (format "%d mismatches found:" (count changes))))
              (println)
              (doseq [{:keys [coord project lib old-tag old-sha new-tag new-sha
                              old-version new-version]} changes]
                (if (= coord :mvn)
                  (printf "  %-25s %-35s %s -> %s  (mvn)\n"
                          (ui/c :cyan project) (str lib)
                          (ui/c :red old-version) (ui/c :green new-version))
                  (printf "  %-25s %-35s %s %s -> %s %s\n"
                          (ui/c :cyan project) (str lib)
                          (ui/c :red old-tag) (ui/c :dim old-sha)
                          (ui/c :green new-tag) (ui/c :dim new-sha))))
              (println)
              (if apply
                (do (apply-sync-changes! root-dir changes)
                    (when commit
                      (git/auto-commit-workspace! root-dir dep-files
                                              "chore: sync internal deps (bb-depsolve)")))
                (println (ui/c :dim "  Dry run. Pass --apply to write changes."))))))))))