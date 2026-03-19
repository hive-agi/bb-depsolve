(ns bb-depsolve.version
  "Pure calculations for version parsing and comparison.

   Layer 1 (Calculation): Zero side effects, zero I/O deps.
   All functions are total over their documented domains.
   Sits at the bottom of the dependency stack — innermost onion layer."
  (:require [clojure.string :as str]))

(defn parse-semver
  "Parse a semver tag like 'v0.4.0' into [major minor patch].
   Returns nil for non-semver strings. Total: never throws."
  [tag]
  (when (string? tag)
    (when-let [[_ major minor patch] (re-matches #"v?(\d+)\.(\d+)\.(\d+).*" tag)]
      [(parse-long major) (parse-long minor) (parse-long patch)])))

(defn pre-release?
  "True if version contains pre-release markers (alpha, beta, rc, snapshot, etc).
   Total: returns false for nil/empty."
  [version]
  (boolean (re-find #"(?i)(alpha|beta|rc|snapshot|SNAPSHOT|milestone|preview)" (str version))))

(defn stable?
  "Complement of pre-release?. True for stable version strings."
  [version]
  (not (pre-release? version)))

(defn parse-version-segments
  "Parse version string into vector of numeric segments.
   Non-numeric chars become segment separators. Total: returns [] for nil."
  [v]
  (if (nil? v)
    []
    (->> (str/split (str/replace (str v) #"[^0-9.]" ".") #"\.")
         (remove str/blank?)
         (mapv #(try (parse-long %) (catch Exception _ 0))))))

(defn version-newer?
  "True if new-v is strictly newer than old-v.
   Handles numeric segments and avoids suggesting downgrades.
   Total: returns false for nil/equal versions."
  [old-v new-v]
  (let [old-parts (parse-version-segments old-v)
        new-parts (parse-version-segments new-v)
        max-len (max (count old-parts) (count new-parts))
        pad (fn [v] (vec (concat v (repeat (- max-len (count v)) 0))))
        old-padded (pad old-parts)
        new-padded (pad new-parts)]
    (pos? (compare new-padded old-padded))))

(defn version-compare
  "Compare two version strings. Returns neg/zero/pos like compare.
   Total: treats nil as [0]."
  [a b]
  (let [pa (parse-version-segments a)
        pb (parse-version-segments b)
        max-len (max (count pa) (count pb))
        pad (fn [v] (vec (concat v (repeat (- max-len (count v)) 0))))]
    (compare (pad pa) (pad pb))))

(defn latest-tag
  "Get the latest semver tag from a list of {:tag :sha} maps.
   Pure: no I/O. Returns nil if no valid semver tags found."
  [tags]
  (->> tags
       (filter #(parse-semver (:tag %)))
       (sort-by #(parse-semver (:tag %)))
       last))

(defn find-git-deps
  "Find all git deps in file content string.
   Returns vec of {:lib :tag :sha :match}. Pure: no I/O."
  [content]
  (let [pattern #"([\w.\-]+/[\w.\-]+)\s+\{[^}]*:git/tag\s+\"([^\"]+)\"\s+:git/sha\s+\"([^\"]+)\""]
    (->> (re-seq pattern content)
         (mapv (fn [[match lib tag sha]]
                 {:lib (symbol lib) :tag tag :sha sha :match match})))))

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

(defn update-shadow-dep
  "Replace a dependency version in shadow-cljs.edn :dependencies vector.
   Handles both [group/artifact \"ver\"] and [artifact \"ver\"] forms.
   Pure: returns new string."
  [content lib-sym new-version]
  (let [lib-str (str lib-sym)
        ;; For unqualified names like reagent/reagent, also match bare [reagent ...]
        names (if (let [[g a] (str/split lib-str #"/")] (= g a))
                [(str/replace lib-str #"/.*" "") lib-str]
                [lib-str])
        escaped-names (map (fn [n]
                             (-> n
                                 (str/replace "." "\\.")
                                 (str/replace "/" "\\/")))
                           names)
        alt-pattern (str/join "|" escaped-names)
        pattern (re-pattern
                 (str "(\\[)(" alt-pattern ")(\\s+\")([^\"]+)(\")"))]
    (str/replace content pattern (str "$1$2$3" new-version "$5"))))

(defn find-local-deps
  "Find all :local/root deps in file content string.
   Returns vec of {:lib :path :match}. Pure: no I/O."
  [content]
  (let [pattern #"([\w.\-]+/[\w.\-]+)\s+\{[^}]*:local/root\s+\"([^\"]+)\"[^}]*\}"]
    (->> (re-seq pattern content)
         (mapv (fn [[match lib path]]
                 {:lib (symbol lib) :path path :match match})))))

(defn replace-local-with-git
  "Replace a :local/root dep with a :git/tag+sha coordinate in content.
   Optionally renames the lib (e.g. hive-mcp/hive-mcp -> io.github.hive-agi/hive-mcp).
   Pure: returns new string."
  ([content lib-sym new-tag new-sha]
   (replace-local-with-git content lib-sym new-tag new-sha nil))
  ([content lib-sym new-tag new-sha new-lib-sym]
   (let [lib-str (str lib-sym)
         escaped (-> lib-str
                     (str/replace "." "\\.")
                     (str/replace "/" "\\/"))
         pattern (re-pattern
                  (str escaped "(\\s+)\\{[^}]*:local/root\\s+\"[^\"]+\"[^}]*\\}"))
         replacement-lib (str (or new-lib-sym lib-sym))]
     (str/replace content pattern
                  (str replacement-lib "$1" "{:git/tag \"" new-tag "\" :git/sha \"" new-sha "\"}")))))

(defn replace-local-with-mvn
  "Replace a :local/root dep with a :mvn/version coordinate in content.
   Pure: returns new string."
  [content lib-sym new-version]
  (let [lib-str (str lib-sym)
        escaped (-> lib-str
                    (str/replace "." "\\.")
                    (str/replace "/" "\\/"))
        pattern (re-pattern
                 (str "(" escaped "\\s+)\\{[^}]*:local/root\\s+\"[^\"]+\"[^}]*\\}"))]
    (str/replace content pattern
                 (str "$1" "{:mvn/version \"" new-version "\"}"))))

(defn update-git-dep
  "Replace a git dep's tag+sha in file content string. Pure: returns new string."
  [content lib-sym new-tag new-sha]
  (let [lib-str (str lib-sym)
        escaped (-> lib-str
                    (str/replace "." "\\.")
                    (str/replace "/" "\\/"))
        tag-pattern (re-pattern
                     (str "(?s)(" escaped "\\s+\\{[^}]*:git/tag\\s+\")([^\"]+)(\"\\s+:git/sha\\s+\")([^\"]+)(\")"))]
    (str/replace content tag-pattern
                 (str "$1" new-tag "$3" new-sha "$5"))))

(defn update-mvn-dep
  "Replace a mvn dep's version in file content string. Pure: returns new string."
  [content lib-sym new-version]
  (let [lib-str (str lib-sym)
        escaped (-> lib-str
                    (str/replace "." "\\.")
                    (str/replace "/" "\\/"))
        pattern (re-pattern
                 (str "(" escaped "\\s+\\{[^}]*:mvn/version\\s+\")([^\"]+)(\")"))]
    (str/replace content pattern (str "$1" new-version "$3"))))

(defn sha-matches?
  "Compare two SHA strings by their common prefix length.
   Handles short vs full SHA comparison."
  [a b]
  (when (and (string? a) (string? b) (pos? (count a)) (pos? (count b)))
    (let [len (min (count a) (count b))]
      (= (subs a 0 len) (subs b 0 len)))))

(defn pick-sha
  "Choose appropriate SHA format: short if old-sha is short, full otherwise."
  [old-sha resolved-info]
  (if (<= (count (str old-sha)) 12)
    (:sha-short resolved-info (:sha resolved-info))
    (:sha resolved-info)))

(defn bump-patch
  "Increment patch version. [0 1 1] -> [0 1 2]"
  [[major minor patch]]
  [major minor (inc patch)])

(defn bump-minor
  "Increment minor, zero patch. [0 1 1] -> [0 2 0]"
  [[major minor _]]
  [major (inc minor) 0])

(defn bump-major
  "Increment major, zero minor+patch. [0 1 1] -> [1 0 0]"
  [[major _ _]]
  [(inc major) 0 0])

(defn semver->tag
  "Format semver triple as tag string. [1 2 3] -> \"v1.2.3\""
  [[major minor patch]]
  (str "v" major "." minor "." patch))

(defn semver->version
  "Format semver triple as version string. [1 2 3] -> \"1.2.3\""
  [[major minor patch]]
  (str major "." minor "." patch))

(defn parse-github-lib
  "Parse io.github.org/repo into {:org :repo}. Returns nil if not a github lib."
  [lib-sym]
  (when-let [[_ org repo] (re-matches #"io\.github\.(.+)/(.+)" (str lib-sym))]
    {:org org :repo repo}))

(defn infer-sibling-dir
  "Infer the sibling directory name from a :local/root path like '../hive-events'.
   Returns the directory basename, or nil if the path is absolute or not a sibling ref."
  [local-path]
  (when (and (string? local-path)
             (str/starts-with? local-path "../"))
    (let [basename (last (str/split local-path #"/"))]
      (when-not (str/blank? basename)
        basename))))

(defn lib-matches-org?
  "True if lib-sym belongs to the given GitHub org."
  [org lib-sym]
  (str/starts-with? (str lib-sym) (str "io.github." org "/")))

(defn lib-artifact-id
  "Extract artifact-id from a qualified lib symbol."
  [lib-sym]
  (last (str/split (str lib-sym) #"/")))
