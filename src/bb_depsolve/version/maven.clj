(ns bb-depsolve.version.maven
  "Maven metadata and POM handling. Pure."
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str]
            [malli.core :as m]
            [bb-depsolve.schema]
            [bb-depsolve.version.lib :as lib]
            [bb-depsolve.version.semver :as semver]))

(defn maven-property?
  "True if s is an unresolved Maven property placeholder like ${foo.bar}.
   Total: returns false for nil/non-string."
  [s]
  (boolean (and (string? s) (re-matches #"\$\{[^}]+\}" s))))

(defn unresolved-property?
  "True if the input still contains an unresolved Maven `${...}` placeholder
   after POM parsing. Pure, total.

   Two arities:
   - (unresolved-property? s)      => true if string s contains `${...}` anywhere
   - (unresolved-property? coord)  => true if any key field (:lib/:group/:artifact/:version)
                                       of the coord map contains a `${...}` placeholder

   Notes:
   - `maven-property?` is strict: whole-string match against `${foo}`.
   - `unresolved-property?` is looser: detects placeholders anywhere (e.g. `foo-${bar}-baz`)
     and understands coord maps directly, which is what the filter step needs.
   - Returns false for nil/non-string/non-map inputs."
  [x]
  (cond
    (string? x)
    (boolean (re-find #"\$\{[^}]+\}" x))

    (map? x)
    (let [fields [:lib :group :artifact :version :group-id :artifact-id]]
      (boolean
       (some (fn [k]
               (let [v (get x k)]
                 (unresolved-property? (when v (str v)))))
             fields)))

    :else false))

(defn filter-resolved-coords
  "Drop coord maps whose key fields still contain `${...}` placeholders.
   Pure, total. Returns a vec preserving input order.

   2-arity variant accepts a warn-fn `(fn [coord] ...)` called once per dropped
   coord. This is how the I/O boundary surfaces diagnostics without the
   Calculation layer taking on a logging dependency (see convention
   20260216171950-24142d26: SLAP)."
  ([coords]
   (filter-resolved-coords coords nil))
  ([coords warn-fn]
   (if (nil? coords)
     []
     (->> coords
          (reduce (fn [acc coord]
                    (if (unresolved-property? coord)
                      (do (when warn-fn (warn-fn coord))
                          acc)
                      (conj acc coord)))
                  [])))))

(defn maven-metadata-url
  "Maven metadata.xml URL for an artifact under a repo base. Pure string.
   base-url e.g. \"https://gitea.hive-mcp.com/api/packages/hive-agi/maven\"."
  [base-url group-id artifact-id]
  (format "%s/%s/%s/maven-metadata.xml"
          (str/replace (str base-url) #"/+$" "")
          (lib/group-id->path group-id) artifact-id))

(defn parse-maven-metadata-versions
  "All <version> strings under <versioning><versions>. Total: [] on nil/unparseable."
  [xml-string]
  (try
    (let [p (xml/parse-str xml-string)
          tag= (fn [el t] (and (map? el) (keyword? (:tag el)) (= (name (:tag el)) t)))
          find1 (fn [parent t] (first (filter #(tag= % t) (:content parent))))
          text (fn [el] (when el (str/trim (apply str (filter string? (:content el))))))
          versioning (find1 p "versioning")
          versions-el (find1 versioning "versions")]
      (->> (:content versions-el) (filter #(tag= % "version")) (mapv text) (remove str/blank?) vec))
    (catch Exception _ [])))

(defn latest-published-version
  "MAX stable version from <versions> (robust to a stale <latest>). Pure.
   opts {:allow-pre? bool}. Returns nil when none."
  [xml-string {:keys [allow-pre?]}]
  (let [vs (parse-maven-metadata-versions xml-string)
        vs (if allow-pre? vs (remove semver/pre-release? vs))]
    (when (seq vs) (last (sort semver/version-compare vs)))))

(defn pom-urls
  "Return [clojars-url maven-central-url] for a Maven artifact.
   Pure: computes URL strings only."
  [group-id artifact-id version]
  (let [gpath (lib/group-id->path group-id)
        fname (str artifact-id "-" version ".pom")]
    [(format "https://repo.clojars.org/%s/%s/%s/%s" gpath artifact-id version fname)
     (format "https://repo1.maven.org/maven2/%s/%s/%s/%s" gpath artifact-id version fname)]))

(defn parse-pom-deps-raw
  "Parse POM XML string, extract compile-scope dependencies WITHOUT filtering
   unresolved Maven `${...}` property placeholders. Use this at the I/O boundary
   together with `filter-resolved-coords` so dropped coords can be logged.

   Returns vec of {:lib :version}. Skips test/provided/system/optional scopes.
   Total: returns [] for nil/unparseable input."
  [xml-string]
  (try
    (let [parsed (xml/parse-str xml-string)
          tag-name (fn [el] (when (keyword? (:tag el)) (keyword (name (:tag el)))))
          find-el (fn [parent tag-kw]
                    (first (filter #(= (tag-name %) tag-kw) (:content parent))))
          text (fn [el] (when el (str/trim (apply str (filter string? (:content el))))))
          deps-el (find-el parsed :dependencies)]
      (if deps-el
        (->> (:content deps-el)
             (filter #(= (tag-name %) :dependency))
             (keep (fn [dep]
                     (let [group (text (find-el dep :groupId))
                           artifact (text (find-el dep :artifactId))
                           version (text (find-el dep :version))
                           scope (or (text (find-el dep :scope)) "compile")
                           optional (text (find-el dep :optional))]
                       (when (and group artifact version
                                  (= scope "compile")
                                  (not= optional "true"))
                         {:lib (symbol (str group "/" artifact))
                          :version version}))))
             (vec))
        []))
    (catch Exception _ [])))

(defn parse-pom-deps
  "Parse POM XML string, extract compile-scope dependencies.
   Returns vec of {:lib :version}. Skips test/provided/system/optional deps
   AND silently drops unresolved Maven `${...}` property placeholders.
   Total: returns [] for nil/unparseable input.

   For visibility into dropped placeholder coords (warnings), use
   `parse-pom-deps-raw` + `filter-resolved-coords` at the I/O boundary."
  [xml-string]
  (filter-resolved-coords (parse-pom-deps-raw xml-string)))

(m/=> parse-maven-metadata-versions
      [:=> [:cat [:maybe :string]] [:vector :string]])

(m/=> latest-published-version
      [:=> [:cat [:maybe :string] [:map [:allow-pre? {:optional true} [:maybe :boolean]]]]
       [:maybe :string]])
