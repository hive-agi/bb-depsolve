(ns bb-depsolve.core.upgrade
  "The upgrade command: mvn dependency upgrades."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.core.resolve.registries :as registries]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.version.api :as v]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-weave.parallel :as par]))

(def ^:private resolve-concurrency
  "Simultaneous registry lookups. Each is one or more HTTP round-trips, so the
   ceiling is remote politeness, not local CPU. Matches core.sync."
  8)

(def ^:private resolve-timeout-ms 30000)

(defn apply-mvn-change!
  "Apply a single mvn version change to file content, dispatching by file type.
   Returns updated content string."
  [content dep-file lib new-version]
  (if (discovery/shadow-deps-file? dep-file)
    (v/update-shadow-dep content lib new-version)
    (v/update-mvn-dep content lib new-version)))

(defn apply-mvn-upgrades!
  "Apply mvn version upgrades to files. Action: writes to disk.
   Dispatches to the correct update fn based on file type."
  [root-dir upgrades dep-file-index]
  (let [by-file (group-by :path upgrades)]
    (doseq [[path file-upgrades] by-file
            :let [content (atom (slurp path))
                  dep-file (get dep-file-index path)]]
      (doseq [{:keys [lib new-version]} file-upgrades]
        (swap! content apply-mvn-change! dep-file lib new-version))
      (spit path @content)
      (println (ui/c :green (str "  Updated " (str (fs/relativize root-dir path))))))
    (println)
    (println (ui/c :green (format "Applied %d upgrades across %d files."
                               (count upgrades) (count by-file))))))

(defn upgrade-cmd
  "Check for newer versions of all dependencies. --project <name> scopes the
   scan to one project; --root may also point directly at a project dir.

   Libraries no registry could resolve are NAMED, not merely counted: an
   unresolved library is a coverage gap (it may have upgrades nobody will see),
   which a bare `Resolved N / M` line hides."
  [{:keys [opts]}]
  (let [{:keys [root apply commit skip-dirs depth pre-release project]
         :or {root "." depth discovery/default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        dep-files (cond->> (discovery/find-dep-files {:root root :skip-dirs skip-set :depth depth})
                    project (filter #(= project (:project %))))
        dep-file-index (into {} (map (fn [df] [(:path df) df]) dep-files))]

    (println (ui/c :bold "Checking latest versions..."))
    (println)

    (let [all-mvn-deps (atom {})
          file-deps (atom [])]

      (doseq [{:keys [path project] :as dep-file} dep-files
              :let [content (slurp path)
                    mvn-deps (discovery/extract-mvn-deps dep-file content)]]
        (doseq [{:keys [lib version]} mvn-deps]
          (swap! all-mvn-deps update lib (fnil conj #{}) version)
          (swap! file-deps conj {:path path :project project
                                 :lib lib :version version})))

      (let [unique-libs (keys @all-mvn-deps)
            _ (printf "  Checking %d unique libraries...\n" (count unique-libs))
            latest-versions (atom {})
            unresolved (atom [])]

        ;; Resolution is one or more remote round-trips per lib and dominates
        ;; upgrade's wall clock, so the lookups run concurrently — the same
        ;; bounded fan-out core.sync uses. A lib that times out or throws
        ;; surfaces in :unresolved rather than vanishing from the report.
        (let [libs (vec (sort-by str unique-libs))
              results (par/bounded-pmap
                       {:concurrency resolve-concurrency
                        :timeout-ms  resolve-timeout-ms
                        :fallback    nil}
                       (fn [lib] (registries/resolve-mvn-latest lib (boolean pre-release)))
                       libs)]
          (doseq [[i lib result] (map vector (range) libs results)]
            (when (zero? (mod i 10))
              (printf "\r  [%d/%d] %s" (inc i) (count libs) (ui/c :dim (str lib)))
              (flush))
            (if (and result (r/ok? result))
              (swap! latest-versions assoc lib (:ok result))
              (swap! unresolved conj lib))))

        (println "\r  " (ui/c :green (format "Resolved %d / %d libraries" (count @latest-versions) (count unique-libs))))
        (when (seq @unresolved)
          (println)
          (println (ui/c :yellow (format "  %d library(ies) NO registry could resolve — upgrades for these are invisible:"
                                         (count @unresolved))))
          (doseq [lib (sort-by str @unresolved)]
            (println (ui/c :dim (str "    " lib)))))
        (println)

        (let [upgrades (->> @file-deps
                            (filter (fn [{:keys [lib version]}]
                                      (let [latest (get @latest-versions lib)]
                                        (and latest
                                             (not= version latest)
                                             (v/version-newer? version latest)))))
                            (mapv (fn [{:keys [path project lib version]}]
                                    {:path path :project project :lib lib
                                     :old-version version
                                     :new-version (get @latest-versions lib)}))
                            (distinct))]

          (if (empty? upgrades)
            (println (ui/c :green "All mvn deps are up to date."))
            (let [by-lib (->> upgrades
                              (group-by :lib)
                              (map (fn [[lib entries]]
                                     (let [e (first entries)]
                                       {:lib lib
                                        :old-version (:old-version e)
                                        :new-version (:new-version e)
                                        :projects (mapv :project entries)})))
                              (sort-by (comp str :lib)))]

              (println (ui/c :yellow (format "%d upgrades available across %d libraries:"
                                          (count upgrades) (count by-lib))))
              (println)

              (doseq [{:keys [lib old-version new-version projects]} by-lib]
                (printf "  %-40s %s -> %s  (%s)\n"
                        (str lib)
                        (ui/c :red old-version)
                        (ui/c :green new-version)
                        (ui/c :dim (str/join ", " projects))))
              (println)

              (if apply
                (let [choices (mapv #(format "%-40s  %s -> %s  (%s)"
                                             (str (:lib %))
                                             (:old-version %)
                                             (:new-version %)
                                             (str/join ", " (:projects %)))
                                    by-lib)
                      selected (or (ui/gum-filter choices
                                               "Select upgrades (tab=toggle, enter=confirm)")
                                   (do (println (ui/c :dim "No TTY — applying all upgrades."))
                                       choices))]
                  (if (empty? selected)
                    (println (ui/c :dim "No upgrades selected."))
                    (let [selected-libs (->> selected
                                             (map #(-> % str/trim (str/split #"\s+" 2) first symbol))
                                             (set))
                          selected-upgrades (filter #(contains? selected-libs (:lib %)) upgrades)]
                      (apply-mvn-upgrades! root-dir selected-upgrades dep-file-index)
                      (when commit
                        (git/auto-commit-workspace! root-dir dep-files
                                                "chore: upgrade deps to latest (bb-depsolve)")))))
                (println (ui/c :dim "  Dry run. Pass --apply for interactive selection."))))))))))
