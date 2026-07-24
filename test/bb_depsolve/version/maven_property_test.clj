(ns bb-depsolve.version.maven-property-test
  "Property tests for Maven metadata and POM handling."
  (:require [bb-depsolve.version :as v]
            [bb-depsolve.version.generators :as g]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.properties :as props]))

(props/defprop-total p22-parse-pom-deps-totality
  v/parse-pom-deps gen/string-alphanumeric)

(defspec p25-maven-property-rejects-plain-versions 200
  (prop/for-all [v g/gen-version-string]
                (false? (v/maven-property? v))))

(defspec p25-maven-property-rejects-semver-tags 200
  (prop/for-all [v g/gen-semver-tag]
                (false? (v/maven-property? v))))

(defspec p25-maven-property-rejects-pre-release 200
  (prop/for-all [v g/gen-pre-release-version]
                (false? (v/maven-property? v))))

(defspec p25-maven-property-accepts-placeholders 200
  (prop/for-all [prop g/gen-maven-property]
                (true? (v/maven-property? prop))))

(defspec p26-unresolved-property-detects-placeholders 200
  (prop/for-all [s g/gen-string-with-placeholder]
                (true? (v/unresolved-property? s))))

(defspec p26-unresolved-property-rejects-semver 200
  (prop/for-all [v g/gen-version-string]
                (false? (v/unresolved-property? v))))

(defspec p26-unresolved-property-rejects-semver-tag 200
  (prop/for-all [v g/gen-semver-tag]
                (false? (v/unresolved-property? v))))

(props/defprop-total p26-unresolved-property-totality
  v/unresolved-property? gen/string-alphanumeric)

(defspec p27-filter-resolved-coords-idempotent 200
  (prop/for-all [coords g/gen-mixed-coords]
                (let [once  (v/filter-resolved-coords coords)
                      twice (v/filter-resolved-coords once)]
                  (= once twice))))

(defspec p27-filter-drops-all-unresolved 200
  (prop/for-all [coords g/gen-mixed-coords]
                (every? (complement v/unresolved-property?)
                        (v/filter-resolved-coords coords))))

(defspec p27-filter-keeps-all-resolved 200
  (prop/for-all [coords (gen/vector g/gen-resolved-coord 0 20)]
                (= coords (v/filter-resolved-coords coords))))

(defspec p27-filter-subset-of-input 200
  (prop/for-all [coords g/gen-mixed-coords]
                (let [filtered (v/filter-resolved-coords coords)]
                  (every? (set coords) filtered))))
