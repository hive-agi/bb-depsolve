(ns bb-depsolve.version.semver
  "Semver arithmetic: parsing, ordering, bumping, tag/version formatting."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [bb-depsolve.schema.api]))

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

(defn tag->mvn-version
  "Convert a git tag to a Maven-style version string by stripping a leading \"v\".
   \"v0.3.6\" -> \"0.3.6\", \"0.3.6\" -> \"0.3.6\". Total: returns nil for nil."
  [tag]
  (when (string? tag)
    (str/replace tag #"^v" "")))

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

(m/=> parse-semver
      [:=> [:cat [:maybe :string]] [:maybe :bb-depsolve/semver-triple]])

(m/=> version-compare
      [:=> [:cat [:maybe :string] [:maybe :string]] :int])

(m/=> latest-tag
      [:=> [:cat [:sequential [:map [:tag :string] [:sha :string]]]]
       [:maybe :bb-depsolve/resolved-lib]])
