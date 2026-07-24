(ns bb-depsolve.wave.lock
  "The lock command: deps.lock.edn per project."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.ui :as ui]
            [bb-depsolve.version :as v]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [hive-dsl.bounded-atom :as ba]))

(defn lock-cmd
  "Generate deps.lock.edn for each workspace project containing the
   resolved transitive dependency tree (Maven nearest-wins).

   Output format (per project):
     {:lock-version 1
      :generated-at <iso-timestamp>
      :source <relative-path-to-deps-file>
      :resolved {<lib> {:version <v> :type <:mvn|:git> :depth <n>}}
      :conflicts {<lib> [<v> ...]}}

   Locks are deterministic given the same inputs. Re-run after upgrade/sync."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs depth tree-depth]
         :or {root "." depth discovery/default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        dep-files (discovery/find-dep-files {:root root :skip-dirs skip-set :depth depth})
        cache (ba/bounded-atom {:max-entries 500})]

    (println (ui/c :bold (format "Generating deps.lock.edn for %d dep files..."
                                    (count dep-files))))
    (println)

    (let [locked (atom 0)]
      (doseq [{:keys [path project] :as df} dep-files
              :let [content (slurp path)
                    mvn-deps (discovery/extract-mvn-deps df content)
                    git-deps (if (discovery/shadow-deps-file? df)
                               []
                               (mapv (fn [{:keys [lib tag]}]
                                       {:lib lib :version tag :type :git})
                                     (v/find-git-deps content)))
                    direct (vec (concat
                                 (mapv (fn [{:keys [lib version]}]
                                         {:lib lib :version version :type :mvn})
                                       mvn-deps)
                                 git-deps))
                    resolve-fn (fn [lib version]
                                 (resolve/resolve-dep-children cache lib version))
                    tree (v/build-dep-tree direct resolve-fn tree-depth)
                    resolution (v/resolve-versions tree)
                    lock-data {:lock-version 1
                               :generated-at (str (java.time.Instant/now))
                               :source (str (fs/relativize root-dir path))
                               :resolved (into (sorted-map)
                                                (for [[lib m] (:resolved resolution)]
                                                  [lib (select-keys m [:version :type :depth])]))
                               :conflicts (into (sorted-map)
                                                 (for [[lib vs] (:conflicts resolution)]
                                                   [lib (vec (sort vs))]))}
                    project-dir (fs/parent path)
                    lock-path (str (fs/path project-dir "deps.lock.edn"))]]
        (spit lock-path (with-out-str (pp/pprint lock-data)))
        (swap! locked inc)
        (println (ui/c :green (format "  %s -> %s"
                                         project
                                         (str (fs/relativize root-dir lock-path))))))

      (println)
      (println (ui/c :bold (format "Locked: %d project(s)" @locked))))))
