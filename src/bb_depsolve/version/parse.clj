(ns bb-depsolve.version.parse
  "Dependency extraction from dep-file content. Read-only, pure."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn find-git-deps
  "Find all git deps in file content string.
   Returns vec of {:lib :tag :sha :match}. Pure: no I/O."
  [content]
  (let [pattern #"([\w.\-]+/[\w.\-]+)\s+\{[^}]*:git/tag\s+\"([^\"]+)\"\s+:git/sha\s+\"([^\"]+)\""]
    (->> (re-seq pattern content)
         (mapv (fn [[match lib tag sha]]
                 {:lib (symbol lib) :tag tag :sha sha :match match})))))

(defn find-git-sha-only-deps
  "Find git deps pinned by :git/sha with no :git/tag in file content string.
   These carry no comparable version and are invisible to tag-based sync.
   Returns vec of {:lib :sha :match}. Pure: no I/O."
  [content]
  (let [tagged (into #{} (map (comp str :lib)) (find-git-deps content))
        pattern #"([\w.\-]+/[\w.\-]+)\s+\{[^}]*:git/sha\s+\"([^\"]+)\""]
    (->> (re-seq pattern content)
         (remove (fn [[_ lib _]] (contains? tagged lib)))
         (mapv (fn [[match lib sha]]
                 {:lib (symbol lib) :sha sha :match match})))))

(defn dep-coords-by-scope
  "Dependency coordinates in deps.edn/bb.edn CONTENT, grouped by scope.
   :runtime holds top-level :deps; :alias holds :deps, :extra-deps and
   :replace-deps of every :aliases and :tasks entry.
   Unparseable content yields empty maps. Pure: no I/O."
  [content]
  (let [m (try (edn/read-string content) (catch Exception _ nil))
        scoped (concat (for [[_ a] (:aliases m) :when (map? a)
                             k [:deps :extra-deps :replace-deps]]
                         (get a k))
                       (for [[_ t] (:tasks m) :when (map? t)
                             k [:deps :extra-deps]]
                         (get t k)))]
    {:runtime (or (:deps m) {})
     :alias (reduce merge {} (filter map? scoped))}))

(defn runtime-libs
  "Set of lib symbols declared under top-level :deps in CONTENT. Pure."
  [content]
  (set (keys (:runtime (dep-coords-by-scope content)))))

(defn find-mvn-deps
  "Find all mvn deps in file content string.
   Returns vec of {:lib :version :match}. Pure: no I/O."
  [content]
  (let [pattern #"([\w.\-]+/[\w.\-]+)\s+\{[^}]*:mvn/version\s+\"([^\"]+)\""]
    (->> (re-seq pattern content)
         (mapv (fn [[match lib version]]
                 {:lib (symbol lib) :version version :match match})))))

(defn find-shadow-deps
  "Find all dependencies in shadow-cljs.edn :dependencies vector.
   Parses Leiningen-style [lib \"version\"] coordinates.
   Returns vec of {:lib :version :match}. Pure: no I/O.

   Handles both qualified (group/artifact) and unqualified (artifact) names.
   Unqualified names are normalized to artifact/artifact (Maven convention)."
  [content]
  (let [pattern #"\[([\w.\-]+(?:/[\w.\-]+)?)\s+\"([^\"]+)\"\s*(?::scope\s+\"[^\"]+\"\s*)?\]"]
    (->> (re-seq pattern content)
         (mapv (fn [[match lib version]]
                 (let [lib-sym (if (str/includes? lib "/")
                                 (symbol lib)
                                 (symbol lib lib))]
                   {:lib lib-sym :version version :match match}))))))

(defn find-local-deps
  "Find all :local/root deps in file content string.
   Returns vec of {:lib :path :match}. Pure: no I/O."
  [content]
  (let [pattern #"([\w.\-]+/[\w.\-]+)\s+\{[^}]*:local/root\s+\"([^\"]+)\"[^}]*\}"]
    (->> (re-seq pattern content)
         (mapv (fn [[match lib path]]
                 {:lib (symbol lib) :path path :match match})))))

(defn deps-edn->dep-coords
  "Parse deps.edn string, extract dependency coordinates from :deps.
   Returns vec of {:lib :version :type} where type is :mvn or :git.
   Total: returns [] for nil/unparseable input."
  [edn-string]
  (try
    (let [parsed (edn/read-string edn-string)
          deps (:deps parsed)]
      (->> deps
           (keep (fn [[lib-sym coord]]
                   (cond
                     (:mvn/version coord)
                     {:lib lib-sym :version (:mvn/version coord) :type :mvn}

                     (:git/tag coord)
                     {:lib lib-sym :version (:git/tag coord) :type :git}

                     :else nil)))
           (vec)))
    (catch Exception _ [])))
