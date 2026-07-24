(ns bb-depsolve.registry-test
  "Tests for bb-depsolve.registry — the live IArtifactRegistry.

   Both registry sources are stubbed at their owning vars, so no test here
   reaches the network."
  (:require [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.port :as port]
            [bb-depsolve.registry :as reg]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(def ^:private carto 'io.github.hive-agi/hive-carto)

(defn- tags
  [& ts]
  (fn [& _] (r/ok (mapv (fn [t] {:tag t :sha "abc1234"}) ts))))

(defn- unavailable [& _] (r/err :io/unavailable))

(defmacro ^:private with-sources
  "Run BODY with the tag source and the maven source stubbed."
  [tag-fn mvn & body]
  `(with-redefs [resolve/resolve-remote-tags ~tag-fn
                 resolve/resolve-mvn-versions (fn [& _#] ~mvn)]
     ~@body))

;; =============================================================================
;; tag-versions
;; =============================================================================

(deftest only-semver-tags-become-versions-test
  (with-sources (tags "v0.1.0" "v0.2.0" "nightly" "v1.0.0-rc1") #{}
    (is (= #{"0.1.0" "0.2.0"} (reg/tag-versions carto))
        "non-semver tags carry no comparable version")))

(deftest an-unreachable-tag-source-yields-no-versions-test
  (with-sources unavailable #{}
    (is (= #{} (reg/tag-versions carto)))))

(deftest a-non-forge-lib-is-not-probed-for-tags-test
  (is (= #{} (reg/tag-versions 'cheshire/cheshire))))

;; =============================================================================
;; known-versions — the union of both sources
;; =============================================================================

(deftest known-versions-unions-tags-and-the-maven-registry-test
  (with-sources (tags "v0.1.0") #{"0.3.0"}
    (is (= #{"0.1.0" "0.3.0"} (reg/known-versions carto false)))))

(deftest either-source-alone-is-enough-test
  (testing "tags only"
    (with-sources (tags "v0.1.0") #{}
      (is (= #{"0.1.0"} (reg/known-versions carto false)))))
  (testing "maven only"
    (with-sources unavailable #{"0.3.0"}
      (is (= #{"0.3.0"} (reg/known-versions carto false))))))

;; =============================================================================
;; IArtifactRegistry — latest-version
;; =============================================================================

(deftest latest-version-orders-by-semver-not-lexicographically-test
  (with-sources (tags "v0.9.0" "v0.10.0" "v0.2.0") #{}
    (is (= "0.10.0" (:ok (port/latest-version (reg/live-registry) carto))))))

(deftest latest-version-is-nil-when-nothing-resolves-test
  (with-sources unavailable #{}
    (is (nil? (:ok (port/latest-version (reg/live-registry) carto))))))

;; =============================================================================
;; IArtifactRegistry — published?
;; =============================================================================

(deftest an-exactly-known-version-is-published-test
  (with-sources (tags "v0.1.0" "v0.2.0") #{}
    (let [reg (reg/live-registry)]
      (is (true? (:ok (port/published? reg carto "0.2.0"))))
      (is (true? (:ok (port/published? reg carto "v0.2.0")))
          "a git tag is normalised before lookup"))))

(deftest a-version-above-everything-known-is-not-published-test
  (with-sources (tags "v0.1.0" "v0.2.0") #{}
    (is (false? (:ok (port/published? (reg/live-registry) carto "0.3.0"))))))

(deftest published-observes-rather-than-infers-test
  (testing "a version no registry enumerated is NOT published, even though it
            sits below the highest known version"
    (with-sources (tags "v0.3.0") #{}
      (is (false? (:ok (port/published? (reg/live-registry) carto "0.1.0")))
          "inferring from 'latest >= wanted' would pass a yanked or never-published version")))
  (testing "and it is published once a registry actually lists it"
    (with-sources (tags "v0.3.0") #{"0.1.0"}
      (is (true? (:ok (port/published? (reg/live-registry) carto "0.1.0")))))))

(deftest nothing-is-published-when-no-source-resolves-test
  (with-sources unavailable #{}
    (is (false? (:ok (port/published? (reg/live-registry) carto "0.1.0"))))))

;; =============================================================================
;; The await contract the executor drives
;; =============================================================================

(deftest await-is-satisfied-once-the-expected-version-resolves-test
  (let [entry {:lib carto :newer-than "0.2.0" :expect "0.3.0"}]
    (with-sources (tags "v0.2.0") #{}
      (is (false? (:ok (port/await-satisfied? (reg/live-registry) entry)))))
    (with-sources (tags "v0.2.0" "v0.3.0") #{}
      (is (true? (:ok (port/await-satisfied? (reg/live-registry) entry)))))))

(deftest await-without-an-expected-version-accepts-anything-newer-test
  (let [entry {:lib carto :newer-than "0.2.0" :expect nil}]
    (with-sources (tags "v0.2.0") #{}
      (is (false? (:ok (port/await-satisfied? (reg/live-registry) entry)))
          "the version already there does not count"))
    (with-sources (tags "v0.2.0" "v0.2.1") #{}
      (is (true? (:ok (port/await-satisfied? (reg/live-registry) entry)))))))

;; =============================================================================
;; allow-pre?
;; =============================================================================

(deftest allow-pre-is-threaded-to-the-maven-source-test
  (let [seen (atom [])]
    (with-redefs [resolve/resolve-remote-tags unavailable
                  resolve/resolve-mvn-versions (fn [_ allow-pre?]
                                                 (swap! seen conj allow-pre?)
                                                 #{"0.3.0"})]
      (port/latest-version (reg/live-registry {:allow-pre? true}) carto)
      (port/latest-version (reg/live-registry) carto)
      (is (= [true nil] @seen)))))
