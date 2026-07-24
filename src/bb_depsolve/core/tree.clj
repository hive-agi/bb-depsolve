(ns bb-depsolve.core.tree
  "The tree command: transitive dependency tree."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.ui :as ui]
            [bb-depsolve.version :as v]
            [clojure.string :as str]
            [hive-dsl.bounded-atom :as ba]))

(defn tree-cmd
  "Show transitive dependency tree with conflict detection.

   Flags:
     --conflicts-only  only print projects that have version conflicts
     --resolved        also print the Maven-style nearest-wins resolution
                       (one chosen version per lib) per project"
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs depth tree-depth conflicts-only resolved]
         :or {root "." depth discovery/default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        dep-files (discovery/find-dep-files {:root root :skip-dirs skip-set :depth depth})
        cache (ba/bounded-atom {:max-entries 500})]

    (println (ui/c :bold "Building dependency tree..."))
    (println)

    (doseq [{:keys [path project] :as dep-file} dep-files
            :let [content (slurp path)
                  mvn-deps (discovery/extract-mvn-deps dep-file content)
                  git-deps (if (discovery/shadow-deps-file? dep-file)
                             []
                             (v/find-git-deps content))]]

      (let [direct-deps (vec (concat
                              (mapv (fn [{:keys [lib version]}]
                                      {:lib lib :version version :type :mvn})
                                    mvn-deps)
                              (mapv (fn [{:keys [lib tag]}]
                                      {:lib lib :version tag :type :git})
                                    git-deps)))
            resolve-fn (fn [lib version]
                         (resolve/resolve-dep-children cache lib version))
            tree (v/build-dep-tree direct-deps resolve-fn tree-depth)
            resolution (v/resolve-versions tree)
            conflicts (:conflicts resolution)]

        (when (or (not conflicts-only) (seq conflicts))
          (println (ui/c :bold (ui/c :cyan project))
                   (ui/c :dim (str " (" (str (fs/relativize root-dir path)) ")")))

          (when-not conflicts-only
            (let [lines (v/format-dep-tree tree conflicts)]
              (doseq [line lines] (println line))))

          (when (seq conflicts)
            (when-not conflicts-only (println))
            (println (ui/c :yellow (str "  " (count conflicts) " conflict(s):")))
            (doseq [[lib versions] (sort-by (comp str key) conflicts)]
              (println (str "    " (ui/c :yellow (str lib)) " — "
                           (str/join " vs " (sort v/version-compare (seq versions)))))))

          (when resolved
            (println)
            (println (ui/c :bold (str "  Resolved (" (count (:resolved resolution)) " libs, nearest-wins):")))
            (doseq [[lib {:keys [version depth]}] (sort-by (comp str key) (:resolved resolution))]
              (println (str "    " (ui/c :cyan (str lib)) " " (ui/c :green version)
                           (ui/c :dim (str "  (depth " depth ")")))))
            (when (seq (:missing resolution))
              (println (ui/c :yellow (str "  Missing (" (count (:missing resolution)) "): "
                                      (str/join ", " (sort (:missing resolution))))))))
          (println))))))
