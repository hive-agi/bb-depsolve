(ns bb-depsolve.version.semver-property-test
  "Property tests for semver arithmetic."
  (:require [bb-depsolve.version.api :as v]
            [bb-depsolve.version.generators :as g]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.properties :as props]))

(props/defprop-total p1-parse-semver-totality
  v/parse-semver gen/string-alphanumeric)

(props/defprop-total p1-pre-release-totality
  v/pre-release? gen/string-alphanumeric)

(props/defprop-total p1-stable-totality
  v/stable? gen/string-alphanumeric)

(props/defprop-total p1-parse-version-segments-totality
  v/parse-version-segments gen/string-alphanumeric)

(props/defprop-complement p2-pre-release-stable-complement
  v/pre-release? v/stable? gen/string-alphanumeric)

(defspec p3-semver-roundtrip 200
  (prop/for-all [triple g/gen-semver-triple]
                (let [[maj min pat] triple
                      tag (str "v" maj "." min "." pat)]
                  (= triple (v/parse-semver tag)))))

(defspec p4-version-newer-irreflexive 200
  (prop/for-all [v g/gen-version-string]
                (not (v/version-newer? v v))))

(defspec p4-version-newer-asymmetric 200
  (prop/for-all [a g/gen-version-string
                 b g/gen-version-string]
    ;; If a < b then NOT b < a
                (if (v/version-newer? a b)
                  (not (v/version-newer? b a))
                  true)))

(defspec p4-version-newer-transitive 100
  (prop/for-all [a g/gen-version-string
                 b g/gen-version-string
                 c g/gen-version-string]
    ;; If a < b and b < c then a < c
                (if (and (v/version-newer? a b) (v/version-newer? b c))
                  (v/version-newer? a c)
                  true)))

(defspec p5-compare-consistent-with-newer 200
  (prop/for-all [a g/gen-version-string
                 b g/gen-version-string]
                (let [cmp (v/version-compare a b)
                      newer (v/version-newer? a b)]
                  (cond
                    (pos? cmp)  (not newer)    ;; a > b implies NOT newer(a,b)
                    (neg? cmp)  newer          ;; a < b implies newer(a,b)
                    (zero? cmp) (not newer)))))

(defspec p6-latest-tag-is-max 200
  (prop/for-all [tags (gen/not-empty (gen/vector g/gen-tag-info))]
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

(defspec p11-pre-release-detects-markers 200
  (prop/for-all [v g/gen-pre-release-version]
                (true? (v/pre-release? v))))

(defspec p11-stable-versions-not-pre-release 200
  (prop/for-all [v g/gen-version-string]
    ;; Pure numeric versions should never be pre-release
                (false? (v/pre-release? v))))

(defspec p15-bump-patch-increments-patch 200
  (prop/for-all [triple g/gen-semver-triple]
                (let [[maj min pat] triple
                      [new-maj new-min new-pat] (v/bump-patch triple)]
                  (and (= maj new-maj)
                       (= min new-min)
                       (= (inc pat) new-pat)))))

(defspec p16-bump-minor-increments-minor 200
  (prop/for-all [triple g/gen-semver-triple]
                (let [[maj min _] triple
                      [new-maj new-min new-pat] (v/bump-minor triple)]
                  (and (= maj new-maj)
                       (= (inc min) new-min)
                       (zero? new-pat)))))

(defspec p17-bump-major-increments-major 200
  (prop/for-all [triple g/gen-semver-triple]
                (let [[maj _ _] triple
                      [new-maj new-min new-pat] (v/bump-major triple)]
                  (and (= (inc maj) new-maj)
                       (zero? new-min)
                       (zero? new-pat)))))

(defspec p18-semver-tag-roundtrip 200
  (prop/for-all [triple g/gen-semver-triple]
                (= triple (v/parse-semver (v/semver->tag triple)))))

(defspec p19-semver-version-roundtrip 200
  (prop/for-all [triple g/gen-semver-triple]
                (= triple (v/parse-semver (v/semver->version triple)))))

(defspec p20-bump-patch-produces-newer 200
  (prop/for-all [triple g/gen-semver-triple]
                (let [old-v (v/semver->version triple)
                      new-v (v/semver->version (v/bump-patch triple))]
                  (v/version-newer? old-v new-v))))

(defspec p20-bump-minor-produces-newer 200
  (prop/for-all [triple g/gen-semver-triple]
                (let [old-v (v/semver->version triple)
                      new-v (v/semver->version (v/bump-minor triple))]
                  (v/version-newer? old-v new-v))))

(defspec p20-bump-major-produces-newer 200
  (prop/for-all [triple g/gen-semver-triple]
                (let [old-v (v/semver->version triple)
                      new-v (v/semver->version (v/bump-major triple))]
                  (v/version-newer? old-v new-v))))
