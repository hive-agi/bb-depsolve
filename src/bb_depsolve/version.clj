(ns bb-depsolve.version
  "Pure calculations for version parsing and comparison.

   Layer 1 (Calculation): Zero side effects, zero I/O deps.
   All functions are total over their documented domains.
   Sits at the bottom of the dependency stack — innermost onion layer."
  (:require [clojure.data.xml :as xml]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.set :as set]
            [malli.core :as m]
            [bb-depsolve.schema]))

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

(defn tag->mvn-version
  "Convert a git tag to a Maven-style version string by stripping a leading \"v\".
   \"v0.3.6\" -> \"0.3.6\", \"0.3.6\" -> \"0.3.6\". Total: returns nil for nil."
  [tag]
  (when (string? tag)
    (str/replace tag #"^v" "")))

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
  (let [git-deps (find-git-deps content)
        mvn-deps (find-mvn-deps content)]
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
                  rversion (tag->mvn-version (:tag resolved-info))]
            :when (and rversion (not= version rversion))]
        {:lib lib :coord :mvn
         :old-version version :new-version rversion})))))

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

(defn parse-forge-lib
  "Parse a forge-prefixed lib symbol into {:forge :org :repo}.
   Supports io.github.*, io.gitlab.*, io.codeberg.*. Pure, total.
   Returns nil for non-forge libs."
  [lib-sym]
  (when-let [[_ forge org repo]
             (re-matches #"io\.(github|gitlab|codeberg)\.([^/]+)/(.+)"
                         (str lib-sym))]
    {:forge (keyword forge) :org org :repo repo}))

(defn forge-clone-url
  "Clone URL for a forge / org / repo. Returns nil for unknown forge.
   Pure: string formatting only."
  [forge org repo]
  (case forge
    :github   (format "https://github.com/%s/%s" org repo)
    :gitlab   (format "https://gitlab.com/%s/%s" org repo)
    :codeberg (format "https://codeberg.org/%s/%s" org repo)
    nil))

(defn forge-raw-url
  "Raw-content URL for FILE at TAG on FORGE. Returns nil for unknown forge.
   Pure: string formatting only."
  [forge org repo tag file]
  (case forge
    :github   (format "https://raw.githubusercontent.com/%s/%s/%s/%s"
                      org repo tag file)
    :gitlab   (format "https://gitlab.com/%s/%s/-/raw/%s/%s"
                      org repo tag file)
    :codeberg (format "https://codeberg.org/%s/%s/raw/tag/%s/%s"
                      org repo tag file)
    nil))

(defn major-bump?
  "True if NEW-TAG is a major version increment from OLD-TAG (per semver).
   For pre-1.0 (major=0), no bumps qualify as major (0.x.x convention).
   Pure, total. Returns false for unparseable inputs."
  [old-tag new-tag]
  (let [old-parts (parse-semver old-tag)
        new-parts (parse-semver new-tag)]
    (boolean
     (and old-parts new-parts
          (pos? (first old-parts))
          (> (first new-parts) (first old-parts))))))

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

(defn canonical-lib
  "The coordinate a :local/root entry must be keyed on.

   A forge-qualified lib is already canonical. Otherwise, when `org` is known
   and `local-path` names a sibling checkout, the canonical coordinate is
   io.github.<org>/<sibling-dir>. Falls back to `lib-sym` unchanged.
   Pure: no I/O."
  [lib-sym local-path org]
  (or (when (parse-github-lib lib-sym) lib-sym)
      (when org
        (when-let [dir (infer-sibling-dir local-path)]
          (symbol (str "io.github." org "/" dir))))
      lib-sym))

;; =============================================================================
;; Transitive dependency resolution (v0.5.0)
;; =============================================================================

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

(defn group-id->path
  "Convert Maven groupId to URL path segment.
   \"com.fasterxml.jackson.core\" -> \"com/fasterxml/jackson/core\"
   Total: returns empty string for nil."
  [group-id]
  (if (string? group-id)
    (str/replace group-id "." "/")
    ""))

(defn maven-metadata-url
  "Maven metadata.xml URL for an artifact under a repo base. Pure string.
   base-url e.g. \"https://gitea.hive-mcp.com/api/packages/hive-agi/maven\"."
  [base-url group-id artifact-id]
  (format "%s/%s/%s/maven-metadata.xml"
          (str/replace (str base-url) #"/+$" "")
          (group-id->path group-id) artifact-id))

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
        vs (if allow-pre? vs (remove pre-release? vs))]
    (when (seq vs) (last (sort version-compare vs)))))

(defn pom-urls
  "Return [clojars-url maven-central-url] for a Maven artifact.
   Pure: computes URL strings only."
  [group-id artifact-id version]
  (let [gpath (group-id->path group-id)
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

(defn build-dep-tree
  "Build a transitive dependency tree from a list of direct deps.
   resolve-fn: (fn [lib version] -> [{:lib :version :type}]) returns children.
   max-depth: recursion limit (0 = direct deps only, no children resolved).
   seen: set of already-visited [lib version] to avoid cycles.
   Returns vec of tree nodes: {:lib :version :type :children [...] :cycle? bool}."
  ([deps resolve-fn max-depth]
   (build-dep-tree deps resolve-fn max-depth 0 #{}))
  ([deps resolve-fn max-depth current-depth seen]
   (->> deps
        (mapv (fn [{:keys [lib version type] :as dep}]
                (let [key [lib version]]
                  (if (or (and max-depth (>= current-depth max-depth))
                          (contains? seen key))
                    (assoc dep :children [] :cycle? (contains? seen key))
                    (let [children (resolve-fn lib version)
                          child-tree (build-dep-tree children resolve-fn max-depth
                                                     (inc current-depth)
                                                     (conj seen key))]
                      (assoc dep :children child-tree :cycle? false)))))))))

(defn find-conflicts
  "Walk a dep tree, find libs appearing with multiple versions.
   Returns map of {lib -> #{versions}} for conflicting libs only."
  [trees]
  (let [versions (atom {})]
    (letfn [(walk [nodes]
              (doseq [{:keys [lib version children]} nodes]
                (swap! versions update lib (fnil conj #{}) version)
                (walk children)))]
      (walk trees))
    (->> @versions
         (filter (fn [[_ vs]] (> (count vs) 1)))
         (into {}))))

(defn collect-occurrences
  "Walk dep tree and collect every occurrence of each lib as
   {lib [{:version :type :depth} ...]}. Pure. Used by resolve-versions."
  [trees]
  (let [acc (atom {})]
    (letfn [(walk [nodes depth]
              (doseq [{:keys [lib version type children cycle?]} nodes]
                (when-not cycle?
                  (swap! acc update lib (fnil conj [])
                         {:version version :type type :depth depth}))
                (walk children (inc depth))))]
      (walk trees 0))
    @acc))

(defn resolve-versions
  "Maven-style nearest-wins resolver over a transitive dep tree.

   Picks ONE chosen version per lib using the occurrence with the lowest
   depth (root deps win over transitive). Ties at the same depth break by
   the highest version (per `version-compare`).

   Pure, total. Inputs:
     trees - vec of dep tree nodes (output of `build-dep-tree`)

   Returns a map:
     {:resolved  {lib {:version :type :depth}}   ; nearest-wins selection
      :conflicts {lib #{versions}}               ; libs with >1 distinct version
      :occurrences {lib [{:version :type :depth} ...]}  ; raw multiset
      :missing   #{libs}}                        ; libs ONLY appearing in cycles,
                                                 ; never resolved elsewhere

   Cycle, conflict, diamond and missing-dep cases are all surfaced through
   this single data structure; callers (Action layer) decide how to render."
  [trees]
  (let [occurrences (collect-occurrences trees)
        cycle-acc (atom #{})
        _ (letfn [(walk [nodes]
                    (doseq [{:keys [lib children cycle?]} nodes]
                      (when cycle? (swap! cycle-acc conj lib))
                      (walk children)))]
            (walk trees))
        cycle-libs @cycle-acc
        nearest (fn [occs]
                  ;; min depth wins; among ties, highest version wins
                  (->> occs
                       (group-by :depth)
                       (sort-by key)
                       first
                       val
                       (sort-by :version version-compare)
                       last))
        resolved (into {}
                       (map (fn [[lib occs]] [lib (nearest occs)]))
                       occurrences)
        conflicts (->> occurrences
                       (filter (fn [[_ occs]]
                                 (> (count (set (map :version occs))) 1)))
                       (map (fn [[lib occs]] [lib (set (map :version occs))]))
                       (into {}))
        missing (set/difference cycle-libs (set (keys resolved)))]
    {:resolved resolved
     :conflicts conflicts
     :occurrences occurrences
     :missing missing}))

(m/=> parse-semver
      [:=> [:cat [:maybe :string]] [:maybe :bb-depsolve/semver-triple]])

(m/=> version-compare
      [:=> [:cat [:maybe :string] [:maybe :string]] :int])

(m/=> latest-tag
      [:=> [:cat [:sequential [:map [:tag :string] [:sha :string]]]]
       [:maybe :bb-depsolve/resolved-lib]])

(m/=> sync-changes-in-content
      [:=> [:cat :string :bb-depsolve/resolved] :bb-depsolve/sync-changes])

(m/=> resolve-versions
      [:=> [:cat [:vector :bb-depsolve/tree-node]] :bb-depsolve/resolution])

(m/=> parse-maven-metadata-versions
      [:=> [:cat [:maybe :string]] [:vector :string]])

(m/=> latest-published-version
      [:=> [:cat [:maybe :string] [:map [:allow-pre? {:optional true} [:maybe :boolean]]]]
       [:maybe :string]])

(defn format-dep-tree
  "Format dependency tree as indented string lines with ANSI colors.
   conflicts: map from find-conflicts. seen: tracks already-printed deps.
   Returns vec of formatted strings (one per line)."
  ([trees conflicts]
   (format-dep-tree trees conflicts 0 (atom #{})))
  ([trees conflicts indent-level seen]
   (let [indent (apply str (repeat (* 2 indent-level) \space))]
     (->> trees
          (mapcat (fn [{:keys [lib version children cycle?]}]
                    (let [conflict? (contains? conflicts lib)
                          seen? (contains? @seen [lib version])
                          _ (swap! seen conj [lib version])
                          version-str (cond
                                        conflict? (str "\033[33m" version "\033[0m")
                                        :else     (str "\033[2m" version "\033[0m"))
                          suffix (cond
                                   cycle?  (str " \033[2m(cycle)\033[0m")
                                   seen?   (str " \033[2m(already seen)\033[0m")
                                   :else   "")
                          line (str indent (str lib) " " version-str suffix)]
                      (if (or cycle? seen?)
                        [line]
                        (into [line]
                              (format-dep-tree children conflicts
                                               (inc indent-level) seen))))))
          (vec)))))

;; =============================================================================
;; CVE / Vulnerability Parsing (v0.6.0)
;; =============================================================================

(defn lib->maven-coord
  "Convert a Clojure lib symbol to Maven groupId:artifactId.
   \"io.grpc/grpc-netty-shaded\" -> [\"io.grpc\" \"grpc-netty-shaded\"]
   \"cheshire/cheshire\" -> [\"cheshire\" \"cheshire\"]
   Total: returns nil for non-Maven-like libs."
  [lib-sym]
  (let [s (str lib-sym)
        [group artifact] (str/split s #"/" 2)]
    (when (and group (seq group))
      [(or group artifact) (or artifact group)])))

(defn parse-osv-vuln
  "Parse a single OSV vulnerability entry into a normalized map.
   Total: returns nil for unparseable input."
  [vuln]
  (when (map? vuln)
    (let [aliases (get vuln :aliases [])
          cve-id (first (filter #(str/starts-with? % "CVE-") aliases))
          ghsa-id (:id vuln)
          severity (get-in vuln [:database_specific :severity])
          fixed-versions (->> (get vuln :affected [])
                              (mapcat (fn [affected]
                                        (->> (get affected :ranges [])
                                             (mapcat (fn [r]
                                                       (->> (get r :events [])
                                                            (keep :fixed)))))))
                              (distinct)
                              (vec))]
      {:id          (or cve-id ghsa-id (:id vuln))
       :cve         cve-id
       :ghsa        ghsa-id
       :summary     (:summary vuln)
       :severity    severity
       :fixed-in    fixed-versions
       :published   (:published vuln)
       :references  (->> (get vuln :references [])
                         (filter #(= "ADVISORY" (:type %)))
                         (mapv :url))})))

(defn severity-rank
  "Numeric rank for severity. Higher = worse. Total: 0 for unknown."
  [severity]
  (case (some-> severity str/upper-case)
    "CRITICAL" 4
    "HIGH"     3
    "MODERATE" 2
    "MEDIUM"   2
    "LOW"      1
    0))

(defn sort-vulns-by-severity
  "Sort vulnerabilities by severity (worst first). Pure."
  [vulns]
  (sort-by (comp - severity-rank :severity) vulns))