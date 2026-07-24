(ns bb-depsolve.port-test
  "Tests for bb-depsolve.port."
  (:require [bb-depsolve.cascade-test :as ct]
            [bb-depsolve.port :as p]
            [bb-depsolve.schema :as sch]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private weave 'io.github.test/weave)
(def ^:private dsl 'io.github.test/dsl)

(defn- pin
  [dep lib to]
  {:dep dep :lib lib :coord :mvn :path (str dep "/deps.edn")
   :from "0.1.0" :to to})

(def ^:private pinned-step
  {:project "weave" :lib weave :dir "weave" :role :seed
   :release-mode :pinned :current-version "0.3.0"
   :bump-kind :patch :next-version "0.3.1"
   :pin-updates [(pin "dsl" dsl "0.1.1")
                 (assoc (pin "spi" 'io.github.test/spi nil) :coord :git)]})

(def ^:private rolling-step
  {:project "carto" :lib 'io.github.test/carto :dir "carto" :role :consumer
   :release-mode :rolling :current-version "0.1.1"
   :bump-kind nil :next-version nil
   :pin-updates []})

;; =============================================================================
;; Unit — sync-pins!
;; =============================================================================

(deftest sync-pins-applies-known-targets-and-skips-the-rest-test
  (let [port (p/memory-port)
        {:keys [ok]} (p/sync-pins! port pinned-step)]
    (is (= ["dsl"] (mapv :dep (:applied ok))))
    (is (= ["spi"] (mapv :dep (:skipped ok)))
        "a pin whose :to is unpredictable is skipped, never guessed")
    (is (= #{"dsl/deps.edn"} (set (:paths ok))))
    (is (= {"dsl/deps.edn" {dsl "0.1.1"}} (:pins @(:state port))))))

(deftest sync-outcome-satisfies-its-schema-test
  (is (sch/validate! :bb-depsolve/sync-outcome
                     (:ok (p/sync-pins! (p/memory-port) pinned-step)))))

;; =============================================================================
;; Unit — release!
;; =============================================================================

(deftest a-pinned-release-advances-to-the-planned-version-test
  (let [port (p/memory-port)
        {:keys [ok]} (p/release! port pinned-step)]
    (is (= "0.3.1" (:version ok)))
    (is (= "v0.3.1" (:tag ok)) "a pinned release is tagged")
    (is (sch/validate! :bb-depsolve/release-outcome ok))))

(deftest a-rolling-release-mints-its-own-version-test
  (let [port (p/memory-port)
        {:keys [ok]} (p/release! port rolling-step)]
    (is (= "0.1.2" (:version ok)) "the push mints the version the plan omits")
    (is (nil? (:tag ok)) "a rolling release carries no tag")))

(deftest the-minting-rule-is-injectable-test
  (let [port (p/memory-port {:mint (constantly "9.9.9")})]
    (is (= "9.9.9" (:version (:ok (p/release! port rolling-step)))))))

(deftest an-injected-failure-surfaces-as-an-error-test
  (let [port (p/memory-port {:fail #{"weave"}})
        result (p/release! port pinned-step)]
    (is (not (:ok result)))
    (is (= :release-failed (:kind (:error result))))
    (is (empty? (:released @(:state port))))))

;; =============================================================================
;; Unit — observation
;; =============================================================================

(deftest an-artifact-resolves-only-after-its-publish-latency-test
  (let [port (p/memory-port {:publish-after 2})]
    (p/release! port pinned-step)
    (is (false? (:ok (p/published? port weave "0.3.1"))) "first poll: pending")
    (is (true? (:ok (p/published? port weave "0.3.1"))) "second poll: resolvable")))

(deftest published-accepts-either-coordinate-shape-test
  (let [port (p/memory-port)]
    (p/release! port pinned-step)
    (is (true? (:ok (p/published? port weave "0.3.1"))))
    (is (true? (:ok (p/published? port weave "v0.3.1")))
        "a git tag is normalized before lookup")))

(deftest latest-version-reports-the-highest-known-test
  (let [port (p/memory-port {:registry {weave #{"0.3.0" "0.10.0" "0.9.0"}}})]
    (is (= "0.10.0" (:ok (p/latest-version port weave)))
        "ordering is semver, not lexicographic")
    (is (nil? (:ok (p/latest-version port 'io.github.test/absent))))))

;; =============================================================================
;; Unit — await-satisfied?
;; =============================================================================

(deftest await-with-a-predicted-version-demands-that-exact-version-test
  (let [port (p/memory-port {:registry {weave #{"0.3.0"}}})
        entry {:lib weave :newer-than "0.3.0" :expect "0.3.1"}]
    (is (false? (:ok (p/await-satisfied? port entry))))
    (p/release! port pinned-step)
    (is (true? (:ok (p/await-satisfied? port entry))))))

(deftest await-without-a-predicted-version-accepts-anything-newer-test
  (testing "a rolling project's version is unknown until the push mints it"
    (let [port (p/memory-port {:registry {'io.github.test/carto #{"0.1.1"}}})
          entry {:lib 'io.github.test/carto :newer-than "0.1.1" :expect nil}]
      (is (false? (:ok (p/await-satisfied? port entry)))
          "the version already there does not count")
      (p/release! port rolling-step)
      (is (true? (:ok (p/await-satisfied? port entry)))))))

(deftest await-on-an-unpublished-lib-is-unsatisfied-not-an-error-test
  (is (false? (:ok (p/await-satisfied? (p/memory-port)
                                       {:lib weave :newer-than nil :expect nil})))))

;; =============================================================================
;; Properties
;; =============================================================================

(defspec releasing-a-wave-satisfies-that-waves-await 50
  (prop/for-all [[_ _ plan] ct/gen-plan]
    (let [port (p/memory-port)]
      (every? (fn [wave]
                (doseq [step (:steps wave)]
                  (p/sync-pins! port step)
                  (p/release! port step))
                (every? #(true? (:ok (p/await-satisfied? port %)))
                        (:libs (:await wave))))
              (:waves plan)))))

(defspec every-planned-project-is-released-exactly-once 50
  (prop/for-all [[_ _ plan] ct/gen-plan]
    (let [port (p/memory-port)
          projects (mapcat #(map :project (:steps %)) (:waves plan))]
      (doseq [wave (:waves plan), step (:steps wave)]
        (p/release! port step))
      (= (set projects) (set (keys (:released @(:state port))))))))
