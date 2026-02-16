(ns bb-depsolve.version
  "Pure calculations for version parsing and comparison.

   Layer 1 (Calculation): Zero side effects, zero I/O deps.
   All functions are total over their documented domains.
   Sits at the bottom of the dependency stack — innermost onion layer."
  (:require [clojure.string :as str]))

;; =============================================================================
;; Semver — Value Object
;; =============================================================================

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

;; =============================================================================
;; Tag Selection — Pure Calculation over tag lists
;; =============================================================================

(defn latest-tag
  "Get the latest semver tag from a list of {:tag :sha} maps.
   Pure: no I/O. Returns nil if no valid semver tags found."
  [tags]
  (->> tags
       (filter #(parse-semver (:tag %)))
       (sort-by #(parse-semver (:tag %)))
       last))

;; =============================================================================
;; Dep Coordinate Parsing — Pure string operations
;; =============================================================================

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

;; =============================================================================
;; SHA Matching — Pure comparison
;; =============================================================================

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

;; =============================================================================
;; Lib Coordinate Parsing — Pure
;; =============================================================================

(defn parse-github-lib
  "Parse io.github.org/repo into {:org :repo}. Returns nil if not a github lib."
  [lib-sym]
  (when-let [[_ org repo] (re-matches #"io\.github\.(.+)/(.+)" (str lib-sym))]
    {:org org :repo repo}))

(defn lib-matches-org?
  "True if lib-sym belongs to the given GitHub org."
  [org lib-sym]
  (str/starts-with? (str lib-sym) (str "io.github." org "/")))

(defn lib-artifact-id
  "Extract artifact-id from a qualified lib symbol."
  [lib-sym]
  (last (str/split (str lib-sym) #"/")))
