(ns bb-depsolve.version.rewrite
  "Dependency coordinate rewriting in dep-file content. Pure."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [bb-depsolve.schema]
            [bb-depsolve.version.parse :as parse]
            [bb-depsolve.version.semver :as semver]))

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

(defn sync-changes-in-content
  "Compute sync changes for a single dep file's CONTENT against RESOLVED
   (map of lib-sym -> {:tag :sha :sha-short}). Pure: no I/O.

   Covers both coord styles:
     :git — {:git/tag :git/sha} drift (tag or sha mismatch)
     :mvn — {:mvn/version} drift against tag->mvn-version of the resolved tag

   Returns vec of change maps, each carrying :coord (:git or :mvn):
     :git -> {:lib :coord :old-tag :old-sha :new-tag :new-sha}
     :mvn -> {:lib :coord :old-version :new-version}"
  [content resolved]
  (let [git-deps (parse/find-git-deps content)
        mvn-deps (parse/find-mvn-deps content)]
    (vec
     (concat
      (for [{:keys [lib tag sha]} git-deps
            :when (contains? resolved lib)
            :let [resolved-info (get resolved lib)
                  rtag (:tag resolved-info)
                  rsha (pick-sha sha resolved-info)]
            :when (or (not= tag rtag) (not (sha-matches? sha rsha)))]
        {:lib lib :coord :git
         :old-tag tag :old-sha sha
         :new-tag rtag :new-sha rsha})
      (for [{:keys [lib version]} mvn-deps
            :when (contains? resolved lib)
            :let [resolved-info (get resolved lib)
                  rversion (semver/tag->mvn-version (:tag resolved-info))]
            :when (and rversion (not= version rversion))]
        {:lib lib :coord :mvn
         :old-version version :new-version rversion})))))

(m/=> sync-changes-in-content
      [:=> [:cat :string :bb-depsolve/resolved] :bb-depsolve/sync-changes])
