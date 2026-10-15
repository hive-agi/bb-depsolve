(ns bb-depsolve.core.tree
  "The tree command: transitive dependency tree."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.resolver :as resolver]
            [bb-depsolve.core.resolver.live :as live]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.version.api :as v]
            [clojure.string :as str]
            [hive-weave.parallel :as par]))

(def ^:private fetch-concurrency
  "Simultaneous POM fetches per tree level. Each one is an HTTP round-trip, so
   the ceiling is set by politeness to the remote, not by local CPU. Matches
   core.sync and core.upgrade."
  8)

(def ^:private fetch-timeout-ms 30000)

(defn fetch-level
  "The :map-fn `build-dep-tree` runs one level's child lookups through:
   bounded concurrent fan-out, with results kept positional. A lookup that
   times out or throws comes back nil, and the tree keeps that dep as
   :unresolved? instead of dropping it."
  [f coll]
  (par/bounded-pmap {:concurrency fetch-concurrency
                     :timeout-ms  fetch-timeout-ms
                     :fallback    nil}
                    f
                    (vec coll)))

(defn unresolved-libs
  "Sorted, distinct [lib version] pairs in TREE whose children could not be
   fetched (nodes `build-dep-tree` marked :unresolved?)."
  [tree]
  (->> (tree-seq map? :children {:children tree})
       (filter :unresolved?)
       (map (juxt :lib :version))
       distinct
       (sort-by str)
       vec))

(defn tree-cmd
  "Show transitive dependency tree with conflict detection.

   RESOLVER is the IVersionResolver children are read from (default
   `live/live-resolver`).

   Flags:
     --conflicts-only  only print projects that have version conflicts
     --resolved        also print the Maven-style nearest-wins resolution
                       (one chosen version per lib) per project"
  ([ctx] (tree-cmd (live/live-resolver) ctx))
  ([resolver {:keys [opts]}]
   (let [{:keys [root skip-dirs depth tree-depth conflicts-only resolved]
          :or {root "." depth discovery/default-depth}} opts
         root-dir (str (fs/canonicalize root))
         skip-set (if skip-dirs
                    (into #{} (str/split skip-dirs #","))
                    discovery/default-skip-dirs)
         dep-files (discovery/find-dep-files {:root root :skip-dirs skip-set :depth depth})
         resolve-fn (fn [lib version]
                      (resolver/children-or-empty resolver lib version))]

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
             tree (v/build-dep-tree direct-deps resolve-fn tree-depth {:map-fn fetch-level})
             resolution (v/resolve-versions tree)
             conflicts (:conflicts resolution)]

         (when (or (not conflicts-only) (seq conflicts))
           (println (ui/c :bold (ui/c :cyan project))
                    (ui/c :dim (str " (" (str (fs/relativize root-dir path)) ")")))

           (when-not conflicts-only
             (let [lines (v/format-dep-tree tree conflicts)]
               (doseq [line lines] (println line))))

           (do (when-let [failed (seq (unresolved-libs tree))] (println (ui/c :yellow (str "  Unresolved (" (count failed) ", children not fetched): " (str/join ", " (map (fn [[l ver]] (str l " " ver)) failed)))))) (when (seq conflicts) (when-not conflicts-only (println)) (println (ui/c :yellow (str "  " (count conflicts) " conflict(s):"))) (doseq [[lib versions] (sort-by (comp str key) conflicts)]
               (println (str "    " (ui/c :yellow (str lib)) " — "
                             (str/join " vs " (sort v/version-compare (seq versions))))))))

           (when resolved
             (println)
             (println (ui/c :bold (str "  Resolved (" (count (:resolved resolution)) " libs, nearest-wins):")))
             (doseq [[lib {:keys [version depth]}] (sort-by (comp str key) (:resolved resolution))]
               (println (str "    " (ui/c :cyan (str lib)) " " (ui/c :green version)
                             (ui/c :dim (str "  (depth " depth ")")))))
             (when (seq (:missing resolution))
               (println (ui/c :yellow (str "  Missing (" (count (:missing resolution)) "): "
                                       (str/join ", " (sort (:missing resolution))))))))
           (println)))))))
