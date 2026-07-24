(ns bb-depsolve.core.report
  "The report command: dependency matrix."
  (:require [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.ui :as ui]
            [bb-depsolve.version :as v]
            [clojure.string :as str]))

(defn report-cmd
  "Show a dependency matrix across all projects."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs depth]
         :or {root "." depth discovery/default-depth}} opts
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        dep-files (discovery/find-dep-files {:root root :skip-dirs skip-set :depth depth})
        matrix (atom (sorted-map))]

    (doseq [{:keys [path project] :as dep-file} dep-files
            :let [content (slurp path)
                  mvn-deps (discovery/extract-mvn-deps dep-file content)
                  git-deps (if (discovery/shadow-deps-file? dep-file)
                             []
                             (v/find-git-deps content))]]
      (doseq [{:keys [lib version]} mvn-deps]
        (swap! matrix assoc-in [lib project] version))
      (doseq [{:keys [lib tag sha]} git-deps]
        (swap! matrix assoc-in [lib project] (str tag " " sha))))

    (let [multi-project (->> @matrix
                             (filter (fn [[_ projs]] (> (count projs) 1)))
                             (into (sorted-map)))
          all-projects (->> (vals multi-project)
                            (mapcat keys)
                            (distinct)
                            (sort))
          csv (ui/matrix->csv multi-project all-projects)
          drift-count (count (filter (fn [[_ pv]] (> (count (set (vals pv))) 1)) multi-project))]

      (println (ui/c :bold "Dependency Matrix"))
      (println (ui/c :bold (format "%d libraries shared, %d with version drift"
                                (count multi-project) drift-count)))
      (println)
      (ui/gum-table csv multi-project all-projects))))
