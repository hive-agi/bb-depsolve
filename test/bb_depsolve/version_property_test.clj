(ns bb-depsolve.version-property-test
  "Property-based tests for bb-depsolve.version (pure Calculation layer).

   Uses hive-test property macros and test.check generators.
   Follows convention: *_property_test.clj for property tests."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.properties :as props]
            [bb-depsolve.version :as v]))

;; =============================================================================
;; Generators — domain-specific, following hive-test parameterization pattern
;; =============================================================================

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

;; =============================================================================
;; P1: Totality — pure functions never throw for valid input
;; =============================================================================

(props/defprop-total p1-parse-semver-totality
  v/parse-semver gen/string-alphanumeric)

(props/defprop-total p1-pre-release-totality
  v/pre-release? gen/string-alphanumeric)

(props/defprop-total p1-stable-totality
  v/stable? gen/string-alphanumeric)

(props/defprop-total p1-parse-version-segments-totality
  v/parse-version-segments gen/string-alphanumeric)

(props/defprop-total p1-find-git-deps-totality
  v/find-git-deps gen/string-alphanumeric)

(props/defprop-total p1-find-mvn-deps-totality
  v/find-mvn-deps gen/string-alphanumeric)

;; =============================================================================
;; P2: Complement — pre-release? and stable? are exact complements
;; =============================================================================

(props/defprop-complement p2-pre-release-stable-complement
  v/pre-release? v/stable? gen/string-alphanumeric)

;; =============================================================================
;; P3: Semver parse roundtrip — parse-semver extracts what we put in
;; =============================================================================

(defspec p3-semver-roundtrip 200
  (prop/for-all [triple gen-semver-triple]
                (let [[maj min pat] triple
                      tag (str "v" maj "." min "." pat)]
                  (= triple (v/parse-semver tag)))))

;; =============================================================================
;; P4: version-newer? strict total order properties
;; =============================================================================

(defspec p4-version-newer-irreflexive 200
  (prop/for-all [v gen-version-string]
                (not (v/version-newer? v v))))

(defspec p4-version-newer-asymmetric 200
  (prop/for-all [a gen-version-string
                 b gen-version-string]
    ;; If a < b then NOT b < a
                (if (v/version-newer? a b)
                  (not (v/version-newer? b a))
                  true)))

(defspec p4-version-newer-transitive 100
  (prop/for-all [a gen-version-string
                 b gen-version-string
                 c gen-version-string]
    ;; If a < b and b < c then a < c
                (if (and (v/version-newer? a b) (v/version-newer? b c))
                  (v/version-newer? a c)
                  true)))

;; =============================================================================
;; P5: version-compare consistent with version-newer?
;; =============================================================================

(defspec p5-compare-consistent-with-newer 200
  (prop/for-all [a gen-version-string
                 b gen-version-string]
                (let [cmp (v/version-compare a b)
                      newer (v/version-newer? a b)]
                  (cond
                    (pos? cmp)  (not newer)    ;; a > b implies NOT newer(a,b)
                    (neg? cmp)  newer          ;; a < b implies newer(a,b)
                    (zero? cmp) (not newer)))));; a = b implies NOT newer

;; =============================================================================
;; P6: latest-tag finds the maximum by semver
;; =============================================================================

(defspec p6-latest-tag-is-max 200
  (prop/for-all [tags (gen/not-empty (gen/vector gen-tag-info))]
                (let [latest (v/latest-tag tags)]
                  (if latest
        ;; latest is >= all others by semver
                    (every? (fn [t]
                              (if-let [sv (v/parse-semver (:tag t))]
                                (>= (compare (v/parse-semver (:tag latest)) sv) 0)
                                true))
                            tags)
        ;; No valid semver tags → none should parse
                    (every? #(nil? (v/parse-semver (:tag %))) tags)))))

;; =============================================================================
;; P7: Dep parsing roundtrip — update then find recovers new values
;; =============================================================================

(defspec p7-update-git-dep-roundtrip 200
  (prop/for-all [{:keys [content lib tag sha]} gen-git-dep-content
                 new-tag gen-semver-tag
                 new-sha gen-sha-short]
                (let [updated (v/update-git-dep content lib new-tag new-sha)
                      deps (v/find-git-deps updated)]
                  (and (= 1 (count deps))
                       (= new-tag (:tag (first deps)))
                       (= new-sha (:sha (first deps)))))))

(defspec p7-update-mvn-dep-roundtrip 200
  (prop/for-all [{:keys [content lib version]} gen-mvn-dep-content
                 new-version gen-version-string]
                (let [updated (v/update-mvn-dep content lib new-version)
                      deps (v/find-mvn-deps updated)]
                  (and (= 1 (count deps))
                       (= new-version (:version (first deps)))))))

;; =============================================================================
;; P8: Idempotency — updating with same values is identity
;; =============================================================================

(defspec p8-update-git-dep-idempotent 200
  (prop/for-all [{:keys [content lib tag sha]} gen-git-dep-content]
                (= content (v/update-git-dep content lib tag sha))))

(defspec p8-update-mvn-dep-idempotent 200
  (prop/for-all [{:keys [content lib version]} gen-mvn-dep-content]
                (= content (v/update-mvn-dep content lib version))))

;; =============================================================================
;; P9: sha-matches? symmetry
;; =============================================================================

(defspec p9-sha-matches-symmetric 200
  (prop/for-all [a gen-sha-short
                 b gen-sha-full]
                (= (boolean (v/sha-matches? a b))
                   (boolean (v/sha-matches? b a)))))

(defspec p9-sha-matches-reflexive 200
  (prop/for-all [sha gen-sha-short]
                (true? (v/sha-matches? sha sha))))

;; =============================================================================
;; P10: parse-github-lib totality and roundtrip
;; =============================================================================

(defspec p10-parse-github-lib-roundtrip 200
  (prop/for-all [org (gen/elements ["hive-agi" "clojure" "babashka"])
                 repo (gen/elements ["core" "fs" "process" "hive-events"])]
                (let [lib (symbol (str "io.github." org "/" repo))
                      parsed (v/parse-github-lib lib)]
                  (and (= org (:org parsed))
                       (= repo (:repo parsed))))))

;; =============================================================================
;; P11: pre-release? correctly identifies all known markers
;; =============================================================================

(defspec p11-pre-release-detects-markers 200
  (prop/for-all [v gen-pre-release-version]
                (true? (v/pre-release? v))))

(defspec p11-stable-versions-not-pre-release 200
  (prop/for-all [v gen-version-string]
    ;; Pure numeric versions should never be pre-release
                (false? (v/pre-release? v))))

;; =============================================================================
;; P12: find-local-deps totality and parsing
;; =============================================================================

(props/defprop-total p12-find-local-deps-totality
  v/find-local-deps gen/string-alphanumeric)

(defspec p12-find-local-deps-roundtrip 200
  (prop/for-all [{:keys [content lib path]} gen-local-dep-content]
                (let [deps (v/find-local-deps content)]
                  (and (= 1 (count deps))
                       (= lib (:lib (first deps)))
                       (= path (:path (first deps)))))))

;; =============================================================================
;; P13: replace-local-with-git roundtrip
;; =============================================================================

(defspec p13-replace-local-with-git-roundtrip 200
  (prop/for-all [{:keys [content lib]} gen-local-dep-content
                 new-tag gen-semver-tag
                 new-sha gen-sha-short]
                (let [updated (v/replace-local-with-git content lib new-tag new-sha)
                      git-deps (v/find-git-deps updated)
                      local-deps (v/find-local-deps updated)]
                  (and (= 1 (count git-deps))
                       (= new-tag (:tag (first git-deps)))
                       (= new-sha (:sha (first git-deps)))
                       (empty? local-deps)))))

;; =============================================================================
;; P14: replace-local-with-mvn roundtrip
;; =============================================================================

(defspec p14-replace-local-with-mvn-roundtrip 200
  (prop/for-all [{:keys [content lib]} gen-local-dep-content
                 new-version gen-version-string]
                (let [updated (v/replace-local-with-mvn content lib new-version)
                      mvn-deps (v/find-mvn-deps updated)
                      local-deps (v/find-local-deps updated)]
                  (and (= 1 (count mvn-deps))
                       (= new-version (:version (first mvn-deps)))
                       (empty? local-deps)))))
