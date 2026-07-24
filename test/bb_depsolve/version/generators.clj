(ns bb-depsolve.version.generators
  "Shared test.check generators for the version submodule suites."
  (:require [clojure.test.check.generators :as gen]))

(def gen-semver-triple
  "Generate a valid semver triple [major minor patch]."
  (gen/tuple (gen/choose 0 99) (gen/choose 0 99) (gen/choose 0 999)))

(def gen-semver-tag
  "Generate a valid semver tag string like 'v1.2.3'."
  (gen/fmap (fn [[maj min pat]]
              (str "v" maj "." min "." pat))
            gen-semver-triple))

(def gen-version-string
  "Generate a version string like '1.2.3' (no v prefix)."
  (gen/fmap (fn [[maj min pat]]
              (str maj "." min "." pat))
            gen-semver-triple))

(def gen-non-semver-string
  "Generate strings that are NOT valid semver."
  (gen/elements ["latest" "nightly" "RELEASE" "main" "abc" "" "v" "..."]))

(def gen-sha-short
  "Generate a 7-char hex SHA."
  (gen/fmap #(apply str (take 7 %))
            (gen/vector (gen/elements "0123456789abcdef") 7)))

(def gen-sha-full
  "Generate a 40-char hex SHA."
  (gen/fmap #(apply str %)
            (gen/vector (gen/elements "0123456789abcdef") 40)))

(def gen-tag-info
  "Generate a {:tag :sha} map with valid semver tag."
  (gen/let [tag gen-semver-tag
            sha gen-sha-short]
    {:tag tag :sha sha}))

(def gen-pre-release-suffix
  (gen/elements ["-alpha" "-beta" "-rc1" "-RC2" "-SNAPSHOT" "-milestone" "-preview"]))

(def gen-pre-release-version
  "Generate a version string with pre-release suffix."
  (gen/fmap (fn [[v suffix]] (str v suffix))
            (gen/tuple gen-version-string gen-pre-release-suffix)))

(def gen-lib-sym
  "Generate a qualified lib symbol like 'org/artifact'."
  (gen/fmap (fn [[g a]] (symbol (str g "/" a)))
            (gen/tuple (gen/elements ["org.clojure" "cheshire" "io.github.hive-agi" "babashka"])
                       (gen/elements ["core" "cheshire" "hive-events" "fs" "process"]))))

(def gen-mvn-dep-content
  "Generate deps.edn content with a single mvn dep."
  (gen/let [lib gen-lib-sym
            version gen-version-string]
    {:content (str lib " {:mvn/version \"" version "\"}")
     :lib lib
     :version version}))

(def gen-git-dep-content
  "Generate deps.edn content with a single git dep."
  (gen/let [lib gen-lib-sym
            tag gen-semver-tag
            sha gen-sha-short]
    {:content (str lib " {:git/tag \"" tag "\" :git/sha \"" sha "\"}")
     :lib lib
     :tag tag
     :sha sha}))

(def gen-local-path
  "Generate a plausible :local/root path."
  (gen/fmap #(str "../" %) (gen/elements ["hive-events" "hive-dsl" "core" "fs" "process"])))

(def gen-local-dep-content
  "Generate deps.edn content with a single :local/root dep."
  (gen/let [lib gen-lib-sym
            path gen-local-path]
    {:content (str lib " {:local/root \"" path "\"}")
     :lib lib
     :path path}))

(def gen-maven-property
  "Generate a Maven property placeholder like ${foo.bar}."
  (gen/fmap (fn [s] (str "${" s "}"))
            (gen/elements ["clojure.version" "project.version" "jackson.version"
                           "foo" "a.b.c" "version"])))

(def gen-string-with-placeholder
  "Generate an arbitrary string that contains at least one ${...} placeholder."
  (gen/let [prefix gen/string-alphanumeric
            inner  (gen/such-that seq gen/string-alphanumeric 25)
            suffix gen/string-alphanumeric]
    (str prefix "${" inner "}" suffix)))

(def gen-resolved-coord
  "Coord map whose :version is a plain semver string."
  (gen/let [lib     gen-lib-sym
            version gen-version-string]
    {:lib lib :version version}))

(def gen-unresolved-coord
  "Coord map whose :version contains an unresolved ${...} placeholder."
  (gen/let [lib   gen-lib-sym
            inner (gen/elements ["clojure.version" "project.version"
                                 "jackson.version" "revision" "foo"])]
    {:lib lib :version (str "${" inner "}")}))

(def gen-mixed-coords
  "Vector of coords mixing resolved and unresolved entries."
  (gen/vector (gen/one-of [gen-resolved-coord gen-unresolved-coord]) 0 20))
