(ns bb-depsolve.version.lib
  "Lib identity and coordinate naming."
  (:require [clojure.string :as str]))

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

(defn group-id->path
  "Convert Maven groupId to URL path segment.
   \"com.fasterxml.jackson.core\" -> \"com/fasterxml/jackson/core\"
   Total: returns empty string for nil."
  [group-id]
  (if (string? group-id)
    (str/replace group-id "." "/")
    ""))

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
