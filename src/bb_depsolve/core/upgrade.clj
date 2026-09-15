(ns bb-depsolve.core.upgrade
  "The upgrade command: mvn dependency upgrades."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.core.resolver :as resolver]
            [bb-depsolve.core.resolver.live :as live]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.version.api :as v]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-weave.parallel :as par]
            [bb-depsolve.core.upgrade.guard :as guard]
            [bb-depsolve.version.repos :as repos]))

(def ^:private resolve-concurrency
  "Simultaneous registry lookups. Each is one or more HTTP round-trips, so the
   ceiling is remote politeness, not local CPU. Matches core.sync."
  8)

(def ^:private resolve-timeout-ms 30000)

(def ^:dynamic *exit!*
  "Called with an exit code when upgrade refuses to write. Default: terminate
   the process."
  (fn [code] (System/exit code)))

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

(defn- print-held!
  "Print the upgrades the guard withheld, one line each, with the reason."
  [held]
  (when (seq held)
    (println (ui/c :yellow (format "%d upgrade(s) HELD, not applied:" (count held))))
    (doseq [{:keys [lib old-version new-version reason registry project]}
            (sort-by (juxt (comp str :lib) :project) held)]
      (printf "  %-40s %s -> %s  %s  (%s)\n"
              (str lib)
              (ui/c :dim old-version)
              (ui/c :dim new-version)
              (ui/c :yellow (case reason
                              :major       "major / pre-1.0 minor jump, pass --allow-major"
                              :downgrade   "refused: moves the pin DOWN"
                              :unreachable (str "only on registry " registry
                                                ", which this project does not declare")
                              (str reason)))
              project))
    (println)))

(defn- choice-line
  [{:keys [lib old-version new-version projects]}]
  (format "%-40s  %s -> %s  (%s)"
          (str lib) old-version new-version (str/join ", " projects)))

(defn- chosen-libs
  "The lib symbols named by the lines a selection returned."
  [selected]
  (->> selected
       (map #(-> % str/trim (str/split #"\s+" 2) first symbol))
       (set)))

(defn upgrade-cmd
  "Check for newer versions of all dependencies. --project <name> scopes the
   scan to one project; --root may also point directly at a project dir.

   RESOLVER is the IVersionResolver latest versions are read from (default
   `live/live-resolver`); each lib's per-registry rows are projected through
   the CONSUMING dep file's own `:mvn/repos`, the same projection `sync` pins
   with, so a project is never moved to a version its registries cannot serve.

   Libraries no registry could resolve are NAMED, not merely counted: an
   unresolved library is a coverage gap (it may have upgrades nobody will see),
   which a bare `Resolved N / M` line hides.

   Flags:
     --only <csv>     upgrade only these libs (qualified, e.g. cheshire/cheshire)
     --exclude <csv>  never upgrade these libs
     --allow-major    release the majors and pre-1.0 minor jumps held by default
     --all            apply every listed upgrade without an interactive pick
     --org <name>     the internal org `sync` owns (default hive-agi); its
                      io.github.<org>/* libs are left to `sync`
     --apply          write the changes; without a TTY it refuses unless --all
                      or --only names the selection
     --commit         auto-commit the changed dep files"
  ([ctx] (upgrade-cmd (live/live-resolver) ctx))
  ([resolver {:keys [opts]}]
   (let [{:keys [root apply commit skip-dirs depth pre-release project org]
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

     (let [file-deps (vec (for [{:keys [path project] :as dep-file} dep-files
                                :let [content (slurp path)
                                      consumer-repos (repos/declared-repos content)]
                                {:keys [lib version]} (discovery/extract-mvn-deps dep-file content)]
                            {:path path :project project :lib lib :version version
                             :consumer-repos consumer-repos}))
           {:keys [kept internal excluded]} (guard/partition-libs (distinct (map :lib file-deps)) opts)]

       (printf "  Checking %d unique libraries...\n" (count kept))
       (when (seq internal)
         (println (ui/c :dim (format "  %d internal library(ies) left to `sync` (io.github.%s/*)."
                                     (count internal) (or org guard/default-org)))))
       (when (seq excluded)
         (println (ui/c :dim (format "  %d library(ies) dropped by --only / --exclude."
                                     (count excluded)))))

       ;; Resolution is one or more remote round-trips per lib and dominates
       ;; upgrade's wall clock, so the lookups run concurrently — the same
       ;; bounded fan-out core.sync uses. A lib that times out or throws
       ;; surfaces in :unresolved rather than vanishing from the report.
       (let [results (par/bounded-pmap
                      {:concurrency resolve-concurrency
                       :timeout-ms  resolve-timeout-ms
                       :fallback    nil}
                      (fn [lib] (resolver/latest-by-registry resolver lib (boolean pre-release)))
                      kept)
             {:keys [latest unresolved]}
             (reduce (fn [acc [i lib result]]
                       (when (zero? (mod i 10))
                         (printf "\r  [%d/%d] %s" (inc i) (count kept) (ui/c :dim (str lib)))
                         (flush))
                       (if (and result (r/ok? result))
                         (assoc-in acc [:latest lib] (vec (:ok result)))
                         (update acc :unresolved conj lib)))
                     {:latest {} :unresolved []}
                     (map vector (range) kept results))]

         (println "\r  " (ui/c :green (format "Resolved %d / %d libraries" (count latest) (count kept))))
         (when (seq unresolved)
           (println)
           (println (ui/c :yellow (format "  %d library(ies) NO registry could resolve — upgrades for these are invisible:"
                                          (count unresolved))))
           (doseq [lib (sort-by str unresolved)]
             (println (ui/c :dim (str "    " lib)))))
         (println)

         (let [{:keys [upgrades held]} (guard/plan file-deps latest opts)]

           (print-held! held)

           (if (empty? upgrades)
             (println (ui/c :green (if (seq held)
                                     "No applicable upgrades: every candidate was held."
                                     "All mvn deps are up to date.")))
             (let [by-lib (->> upgrades
                               (group-by (juxt :lib :old-version :new-version))
                               (map (fn [[[lib old-version new-version] entries]]
                                      {:lib lib
                                       :old-version old-version
                                       :new-version new-version
                                       :projects (mapv :project entries)}))
                               (sort-by (juxt (comp str :lib) :new-version)))]

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
                 (let [choices (mapv choice-line by-lib)
                       selected (if (= :explicit (guard/apply-mode opts))
                                  choices
                                  (ui/gum-filter choices
                                                 "Select upgrades (tab=toggle, enter=confirm)"))]
                   (cond
                     (nil? selected)
                     (do (println (ui/c :red "Refusing --apply: no upgrades were selected and there is no TTY to choose them."))
                         (println (ui/c :dim "  Pass --all to apply every upgrade listed above, or --only <lib,lib> to name them."))
                         (*exit!* 1))

                     (empty? selected)
                     (println (ui/c :dim "No upgrades selected."))

                     :else
                     (let [picked (chosen-libs selected)
                           selected-upgrades (filterv #(contains? picked (:lib %)) upgrades)]
                       (apply-mvn-upgrades! root-dir selected-upgrades dep-file-index)
                       (when commit
                         (git/auto-commit-workspace! root-dir dep-files
                                                     "chore: upgrade deps to latest (bb-depsolve)")))))
                 (println (ui/c :dim "  Dry run. Pass --apply with --all or --only <libs> to write.")))))))))))
