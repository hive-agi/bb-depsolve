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
            [hive-weave.parallel :as par]))

(defn discover-internal-libs
  "Auto-discover internal deps by scanning dep files for io.github.{org}/* coords,
   in both :git/tag+:git/sha and :mvn/version form.
   Returns map of lib-sym -> dir-name."
  [dep-files org]
  (->> dep-files
       (remove discovery/shadow-deps-file?)
       (mapcat (fn [{:keys [path]}]
                 (let [content (slurp path)]
                   (concat (v/find-git-deps content)
                           (v/find-mvn-deps content)))))
       (filter #(v/lib-matches-org? org (:lib %)))
       (map (fn [{:keys [lib]}]
              [lib (v/lib-artifact-id lib)]))
       (into {})))

(def ^:private resolve-concurrency
  "Simultaneous tag lookups. Each is a `git ls-remote` (or a registry call), so
   the ceiling is remote politeness, not local CPU."
  8)

(def ^:private resolve-timeout-ms 30000)

(defn resolve-internal-libs
  "Latest tag+sha for every internal lib, resolved with bounded fan-out.
   Returns {:resolved {lib-sym {:tag :sha ...}} :failed [{:lib :error}]}.

   Resolution is one remote round-trip per lib and dominates sync's wall clock,
   so the lookups run concurrently. A lib that times out or throws surfaces in
   :failed as :io/resolve-failed rather than vanishing from the report."
  [root-dir internal-libs]
  (let [entries (vec internal-libs)
        results (par/bounded-pmap
                 {:concurrency resolve-concurrency
                  :timeout-ms  resolve-timeout-ms
                  :fallback    nil}
                 (fn [[lib-sym dir-name]]
                   (resolve/resolve-lib-tags root-dir lib-sym dir-name))
                 entries)]
    (reduce (fn [acc [[lib-sym _] result]]
              (if (and result (r/ok? result))
                (assoc-in acc [:resolved lib-sym] (:ok result))
                (update acc :failed conj
                        {:lib lib-sym
                         :error (if result (:error result) :io/resolve-failed)})))
            {:resolved {} :failed []}
            (map vector entries results))))

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
      (println (ui/c :bold (format "Resolving %s tags (%d libs)..." (str "io.github." org) (count internal-libs))))
      (println)

      (let [{:keys [resolved failed]} (resolve-internal-libs root-dir internal-libs)]

        (doseq [[lib-sym {:keys [tag sha-short sha source]}] (sort-by (comp str key) resolved)]
          (printf "  %-40s %s -> %s  (%s)\n"
                  (ui/c :cyan (str lib-sym))
                  (ui/c :green tag)
                  (ui/c :dim (or sha-short sha))
                  (name source)))
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