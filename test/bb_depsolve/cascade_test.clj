(ns bb-depsolve.cascade-test
  "Unit and property tests for bb-depsolve.cascade.plan (pure Pipeline layer).

   The properties are the safety contract of a release plan: it never plans a
   downgrade, it never releases a project before a dependency it must re-pin,
   and it is a deterministic function of its inputs."
  (:require [bb-depsolve.cascade.plan :as cas]
            [bb-depsolve.graph.dag :as g]
            [bb-depsolve.graph-test :as gt]
            [bb-depsolve.schema.api :as sch]
            [bb-depsolve.version.api :as v]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [bb-depsolve.cascade.bump :as cbump]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- node
  [p mode version]
  {:project p :lib (symbol "io.github.test" p) :dir p
   :release-mode mode :version version})

(defn- pin
  [from to coord version]
  {:project from :dep to :lib (symbol "io.github.test" to)
   :coord coord :version version :path (str from "/deps.edn") :scope :runtime})

(def ^:private fleet
  "weave <- system <- carto, with carto on the rolling release model."
  (g/dep-graph [(node "weave" :pinned "0.3.0")
                (node "system" :pinned "0.2.8")
                (node "carto" :rolling "0.1.1")]
               [(pin "system" "weave" :mvn "0.3.0")
                (pin "carto" "system" :mvn "0.2.8")
                (pin "carto" "weave" :git "v0.3.0")]))

;; =============================================================================
;; Unit — bump rule chain (OCP)
;; =============================================================================

(deftest rolling-projects-carry-no-bump-test
  (is (nil? (cbump/select-bump-kind {:role :seed :release-mode :rolling
                                   :requested-bump :major}))
      "a rolling project's version is derived, so no bump is planned"))

(deftest seed-honours-the-requested-bump-test
  (is (= :major (cbump/select-bump-kind {:role :seed :release-mode :pinned
                                       :requested-bump :major})))
  (is (= :patch (cbump/select-bump-kind {:role :seed :release-mode :pinned
                                       :requested-bump :patch}))))

(deftest consumer-of-a-major-takes-a-minor-test
  (is (= :minor (cbump/select-bump-kind {:role :consumer :release-mode :pinned
                                       :requested-bump :major
                                       :upstream-bump :major})))
  (is (= :patch (cbump/select-bump-kind {:role :consumer :release-mode :pinned
                                       :requested-bump :major
                                       :upstream-bump :patch}))
      "a consumer does not inherit the seed's request, only its upstream's bump"))

(deftest the-rule-chain-is-open-for-extension-test
  (let [rules (into [{:name :freeze-everything
                      :when #(= "frozen" (:project %))
                      :bump nil}]
                    cbump/default-bump-rules)]
    (is (nil? (cbump/select-bump-kind rules {:project "frozen" :role :seed
                                           :release-mode :pinned
                                           :requested-bump :major})))
    (is (= :major (cbump/select-bump-kind rules {:project "other" :role :seed
                                               :release-mode :pinned
                                               :requested-bump :major}))
        "prepending a rule leaves the default chain intact")))

(deftest strongest-bump-test
  (is (= :major (cbump/strongest-bump [:patch :major :minor])))
  (is (= :patch (cbump/strongest-bump [nil :patch])))
  (is (nil? (cbump/strongest-bump [])))
  (is (nil? (cbump/strongest-bump [nil nil]))))

;; =============================================================================
;; Unit — version arithmetic
;; =============================================================================

(deftest next-version-test
  (is (= "0.5.9" (cbump/next-version "0.5.8" :patch)))
  (is (= "0.6.0" (cbump/next-version "0.5.8" :minor)))
  (is (= "1.0.0" (cbump/next-version "0.5.8" :major)))
  (is (nil? (cbump/next-version "0.5.8" nil)))
  (is (nil? (cbump/next-version "not-a-version" :patch))))

(deftest effective-version-prefers-the-highest-known-test
  (testing "a VERSION file behind the pins does not plan a downgrade"
    (let [{:keys [version observed declared]} (cas/effective-version fleet "system" "0.2.8")]
      (is (= "0.2.8" version))
      (is (= "0.2.8" declared))
      (is (nil? observed) "pins agree with the declared version")))
  (testing "a pin ahead of the VERSION file wins and is reported"
    (let [drifted (g/dep-graph [(node "weave" :pinned "0.3.0") (node "system" :pinned "0.2.8")]
                               [(pin "system" "weave" :mvn "0.3.0")
                                (pin "weave" "system" :mvn "0.2.11")])
          {:keys [version observed]} (cas/effective-version drifted "system" "0.2.8")]
      (is (= "0.2.11" version))
      (is (= "0.2.11" observed)))))

;; =============================================================================
;; Unit — plan shape
;; =============================================================================

(deftest plan-orders-waves-and-fills-pin-updates-test
  (let [plan (cas/plan-cascade fleet #{"weave"})]
    (is (= ["weave" "system" "carto"] (cas/plan-projects plan)))
    (is (= [["weave"] ["system"] ["carto"]]
           (mapv #(mapv :project (:steps %)) (:waves plan))))
    (testing "the seed advances, the consumer re-pins it"
      (let [seed (first (:steps (first (:waves plan))))
            consumer (first (:steps (second (:waves plan))))]
        (is (= :seed (:role seed)))
        (is (= "0.3.1" (:next-version seed)))
        (is (= :consumer (:role consumer)))
        (is (= [{:dep "weave" :lib 'io.github.test/weave :coord :mvn
                 :path "system/deps.edn" :from "0.3.0" :to "0.3.1"}]
               (:pin-updates consumer)))))
    (testing "a git pin is rewritten in tag form, a maven pin in bare form"
      (let [rolling (first (:steps (nth (:waves plan) 2)))]
        (is (= #{"v0.3.1" "0.2.9"} (set (map :to (:pin-updates rolling)))))))))

(deftest rolling-consumers-are-released-without-a-bump-test
  (let [plan (cas/plan-cascade fleet #{"weave"})
        rolling (first (:steps (nth (:waves plan) 2)))]
    (is (= :rolling (:release-mode rolling)))
    (is (nil? (:bump-kind rolling)))
    (is (nil? (:next-version rolling))
        "the version is minted by the push, so it cannot be predicted")))

(deftest await-defaults-to-waiting-and-lists-the-wave-artifacts-test
  (let [plan (cas/plan-cascade fleet #{"weave"})
        await (:await (first (:waves plan)))]
    (is (= :wait (:mode await)))
    (is (= cas/default-await-timeout-ms (:timeout-ms await)))
    (is (= [{:lib 'io.github.test/weave :newer-than "0.3.0" :expect "0.3.1"}]
           (:libs await)))))

(deftest await-can-be-skipped-test
  (let [plan (cas/plan-cascade fleet #{"weave"} {:await {:mode :skip}})]
    (is (every? #(= :skip (:mode (:await %))) (:waves plan)))
    (is (= :skip (get-in plan [:policy :await :mode])))))

(deftest unknown-seeds-are-reported-not-swallowed-test
  (let [plan (cas/plan-cascade fleet #{"weave" "ghost"})]
    (is (= #{"ghost"} (:unknown-seeds plan)))
    (is (= ["weave" "system" "carto"] (cas/plan-projects plan)))))

(deftest cycles-are-excluded-with-a-reason-test
  (let [cyclic (g/dep-graph [(node "a" :pinned "0.1.0")
                             (node "b" :pinned "0.1.0")
                             (node "c" :pinned "0.1.0")
                             (node "seed" :pinned "0.1.0")]
                            [(pin "a" "b" :mvn "0.1.0")
                             (pin "b" "a" :mvn "0.1.0")
                             (pin "a" "seed" :mvn "0.1.0")
                             (pin "c" "a" :mvn "0.1.0")])
        plan (cas/plan-cascade cyclic #{"seed"})]
    (is (= ["seed"] (cas/plan-projects plan)))
    (is (= [#{"a" "b"}] (:cycles plan)))
    (is (= {"a" :cycle-member "b" :cycle-member "c" :blocked-by-cycle}
           (into {} (map (juxt :project :reason)) (:excluded plan))))))

(deftest plan-validates-against-its-schema-test
  (is (sch/validate! :bb-depsolve/cascade-plan (cas/plan-cascade fleet #{"weave"})))
  (is (sch/validate! :bb-depsolve/cascade-plan
                     (cas/plan-cascade fleet #{"weave"} {:requested-bump :major
                                                         :await {:mode :skip}}))))

;; =============================================================================
;; Properties
;; =============================================================================

(def gen-plan
  (gen/let [[gr seeds] gt/gen-dag+seeds
            bump (gen/elements cbump/bump-kinds)]
    [gr seeds (cas/plan-cascade gr seeds {:requested-bump bump})]))

(defspec a-plan-never-schedules-a-downgrade 100
  (prop/for-all [[_ _ plan] gen-plan]
    (every? (fn [{:keys [current-version next-version]}]
              (or (nil? next-version)
                  (v/version-newer? current-version next-version)))
            (mapcat :steps (:waves plan)))))

(defspec a-dependency-is-released-before-the-consumer-that-repins-it 100
  (prop/for-all [[_ _ plan] gen-plan]
    (let [level (into {} (for [w (:waves plan), s (:steps w)]
                           [(:project s) (:index w)]))]
      (every? (fn [w]
                (every? (fn [s]
                          (every? #(< (long (get level (:dep %))) (long (:index w)))
                                  (:pin-updates s)))
                        (:steps w)))
              (:waves plan)))))

(defspec every-planned-project-is-reachable-from-a-seed 100
  (prop/for-all [[gr seeds plan] gen-plan]
    (let [closure (g/downstream-closure gr seeds)]
      (every? closure (cas/plan-projects plan)))))

(defspec planning-is-deterministic 50
  (prop/for-all [[gr seeds] gt/gen-dag+seeds]
    (= (cas/plan-cascade gr seeds) (cas/plan-cascade gr seeds))))

(defspec a-plan-always-satisfies-its-schema 50
  (prop/for-all [[_ _ plan] gen-plan]
    (some? (sch/validate! :bb-depsolve/cascade-plan plan))))
