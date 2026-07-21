(ns bb-depsolve.schema-test
  "Schema-driven tests: registry population, sample validation, malli-gen
   properties, and a mutation facet over sync-changes-in-content."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [malli.generator :as mg]
            [hive-schemas.schema :as hs]
            [hive-test.mutation :as mut]
            [hive-test.mutation.combinators :as mc]
            [bb-depsolve.schema :as sch]
            [bb-depsolve.version :as v]
            [clojure.string :as str]
            [clojure.test.check.generators :as gen]))

;; =============================================================================
;; Registry + sample validation
;; =============================================================================

(deftest registry-populated-test
  (is (= (set (keys sch/schemas))
         (set (sch/register!)))))

(deftest sample-validation-test
  (testing "semver-triple"
    (is (hs/validate :bb-depsolve/semver-triple [0 3 6]))
    (is (not (hs/validate :bb-depsolve/semver-triple [0 3]))))
  (testing "semver-tag / version-string"
    (is (hs/validate :bb-depsolve/semver-tag "v0.3.6"))
    (is (not (hs/validate :bb-depsolve/semver-tag "0.3.6")))
    (is (hs/validate :bb-depsolve/version-string "0.3.6"))
    (is (not (hs/validate :bb-depsolve/version-string "v0.3.6"))))
  (testing "sha"
    (is (hs/validate :bb-depsolve/sha "2fe7a14"))
    (is (hs/validate :bb-depsolve/sha "f825be40bee4b3a45b67fc2e57d2f779406d9f54"))
    (is (not (hs/validate :bb-depsolve/sha "XYZ"))))
  (testing "sync-change multi dispatches on :coord"
    (is (hs/validate :bb-depsolve/sync-change
                     {:lib 'a/b :coord :git :old-tag "v1" :old-sha "abc"
                      :new-tag "v2" :new-sha "def"}))
    (is (hs/validate :bb-depsolve/sync-change
                     {:lib 'a/b :coord :mvn :old-version "0.3.0" :new-version "0.3.6"}))
    (is (not (hs/validate :bb-depsolve/sync-change
                          {:lib 'a/b :coord :mvn :old-version 1 :new-version "0.3.6"}))))
  (testing "resolved-lib tolerates missing :sha-short (local tags)"
    (is (hs/validate :bb-depsolve/resolved-lib {:tag "v0.3.6" :sha "2fe7a14"}))))

(deftest validate!-fail-loud-test
  (is (thrown? clojure.lang.ExceptionInfo
               (sch/validate! :bb-depsolve/sync-change {:lib 'a/b :coord :mvn})))
  (is (= [1 2 3] (sch/validate! :bb-depsolve/semver-triple [1 2 3]))))

;; =============================================================================
;; Malli-gen properties (generators derived from the domain schemas)
;; =============================================================================

(def gen-semver-triple-nonneg
  (mg/generator [:tuple [:int {:min 0 :max 99}] [:int {:min 0 :max 99}] [:int {:min 0 :max 999}]]))

(def gen-semver-tag
  (gen/fmap (fn [[a b c]] (str "v" a "." b "." c)) gen-semver-triple-nonneg))

(def gen-version-string
  (gen/fmap (fn [[a b c]] (str a "." b "." c)) gen-semver-triple-nonneg))

(defspec p-semver-tag-roundtrip 100
  (prop/for-all [triple gen-semver-triple-nonneg]
                (= triple (v/parse-semver (v/semver->tag triple)))))

(defspec p-tag->mvn-version-strips-one-v 100
  (prop/for-all [tag gen-semver-tag]
                (and (= tag (str "v" (v/tag->mvn-version tag)))
                     (not (str/starts-with? (v/tag->mvn-version tag) "v")))))

(defn- sign [n] (cond (neg? n) -1 (pos? n) 1 :else 0))

(defspec p-version-compare-antisymmetric 100
  (prop/for-all [a gen-version-string
                 b gen-version-string]
                (= (sign (v/version-compare a b))
                   (- (sign (v/version-compare b a))))))

(defspec p-version-compare-reflexive 100
  (prop/for-all [a gen-version-string]
                (zero? (v/version-compare a a))))

;; =============================================================================
;; Mutation facet — sync-changes-in-content
;; =============================================================================

(def ^:private git-drift-content
  "{:deps {io.github.hive-agi/hive-test {:git/tag \"v0.3.0\" :git/sha \"763e4bc\"}}}")

(def ^:private mvn-drift-content
  "{:deps {io.github.hive-agi/hive-test {:mvn/version \"0.3.0\"}}}")

(def ^:private resolved
  '{io.github.hive-agi/hive-test {:tag "v0.3.6" :sha "2fe7a14aaaa" :sha-short "2fe7a14"}})

(defn- sync-change-assertions []
  (let [git-changes (v/sync-changes-in-content git-drift-content resolved)
        mvn-changes (v/sync-changes-in-content mvn-drift-content resolved)]
    (testing "outputs conform to the domain schema"
      (is (hs/validate :bb-depsolve/sync-changes git-changes))
      (is (hs/validate :bb-depsolve/sync-changes mvn-changes)))
    (testing "drift is detected, not dropped"
      (is (= 1 (count git-changes)))
      (is (= 1 (count mvn-changes))))
    (testing "semantic anchor: exact change maps"
      (is (= :git (:coord (first git-changes))))
      (is (= "v0.3.6" (:new-tag (first git-changes))))
      (is (= :mvn (:coord (first mvn-changes))))
      (is (= "0.3.6" (:new-version (first mvn-changes)))))
    (testing "every emitted :lib is present in resolved"
      (is (every? #(contains? resolved (:lib %)) git-changes))
      (is (every? #(contains? resolved (:lib %)) mvn-changes)))))

(mut/deftest-mutations sync-changes-mutations-caught
  bb-depsolve.version/sync-changes-in-content
  [(mc/always [])
   (mc/echo-arg 0)]
  sync-change-assertions)
