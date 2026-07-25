(ns bb-depsolve.exec-test
  "Tests for bb-depsolve.exec."
  (:require [bb-depsolve.cascade.plan :as cas]
            [bb-depsolve.cascade-test :as ct]
            [bb-depsolve.release.exec :as exec]
            [bb-depsolve.graph.dag :as g]
            [bb-depsolve.release.port :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- node
  [project mode version]
  {:project project :lib (symbol "io.github.test" project) :dir project
   :release-mode mode :version version})

(defn- pin
  [from to coord version]
  {:project from :dep to :lib (symbol "io.github.test" to)
   :coord coord :version version :path (str from "/deps.edn") :scope :runtime})

(def ^:private fleet
  (g/dep-graph [(node "weave" :pinned "0.3.0")
                (node "mid" :rolling "0.2.4")
                (node "top" :pinned "0.1.0")]
               [(pin "mid" "weave" :mvn "0.3.0")
                (pin "top" "mid" :git "v0.2.4")]))

(def ^:private plan (cas/plan-cascade fleet #{"weave"}))

(def ^:private quiet
  {:emit (constantly nil) :await-opts {:emit (constantly nil)}})

(defn- run
  ([] (run (p/memory-port) plan {}))
  ([port] (run port plan {}))
  ([port a-plan opts]
   [port (exec/run-plan! port port a-plan (merge quiet opts))]))

;; =============================================================================
;; Unit — preflight
;; =============================================================================

(deftest a-clean-plan-passes-preflight-test
  (is (= plan (:ok (exec/preflight plan false)))))

(deftest a-cyclic-plan-is-refused-test
  (let [cyclic (assoc plan :cycles [#{"a" "b"}])
        {:keys [error]} (exec/preflight cyclic false)]
    (is (= :exec/unsafe-plan (:kind error)))
    (is (= [#{"a" "b"}] (:cycles error)))
    (is (str/includes? (exec/format-failure error) "a <-> b"))
    (is (str/includes? (exec/format-failure error) "--force"))))

(deftest unknown-seeds-are-refused-test
  (let [{:keys [error]} (exec/preflight (assoc plan :unknown-seeds #{"ghost"}) false)]
    (is (= ["ghost"] (:unknown-seeds error)))
    (is (str/includes? (exec/format-failure error) "ghost"))))

(deftest force-admits-an-unsafe-plan-test
  (is (some? (:ok (exec/preflight (assoc plan :cycles [#{"a" "b"}]) true))))
  (let [[_ result] (run (p/memory-port) (assoc plan :unknown-seeds #{"ghost"})
                        {:force? true})]
    (is (= :complete (:status (:ok result))))))

;; =============================================================================
;; Unit — a full run
;; =============================================================================

(deftest a-run-releases-every-wave-in-order-test
  (let [[port result] (run)
        {:keys [status waves released]} (:ok result)]
    (is (= :complete status))
    (is (= {"weave" "0.3.1" "mid" "0.2.5" "top" "0.1.1"} released))
    (is (= [["weave"] ["mid"] ["top"]]
           (mapv #(mapv :project (:steps %)) waves)))
    (is (= ["weave" "mid" "top"]
           (mapv second (filter #(= :release (first %)) (:log @(:state port))))))))

(deftest a-rolling-dependency-is-repinned-with-the-version-its-push-minted-test
  (let [[port result] (run)
        top (first (:steps (nth (:waves (:ok result)) 2)))]
    (is (= "0.2.5" (get-in (:ok result) [:released "mid"]))
        "the plan could not predict this — the push mints it")
    (is (= [{:dep "mid" :lib 'io.github.test/mid :coord :git
             :path "top/deps.edn" :from "v0.2.4" :to "v0.2.5"}]
           (:pin-updates top))
        "and the git pin is rewritten in tag form")
    (is (= {'io.github.test/mid "v0.2.5"}
           (get-in @(:state port) [:pins "top/deps.edn"])))))

(deftest resolve-pins-fills-only-what-is-still-unknown-test
  (let [step {:pin-updates [{:dep "a" :coord :mvn :to "1.0.0"}
                            {:dep "b" :coord :git :to nil}
                            {:dep "c" :coord :mvn :to nil}]}]
    (is (= ["1.0.0" "v2.0.0" nil]
           (mapv :to (:pin-updates (exec/resolve-pins step {"b" "2.0.0"}))))
        "an unreleased dependency stays nil rather than being invented")))

;; =============================================================================
;; Unit — await wiring
;; =============================================================================

(deftest a-wave-await-learns-the-version-the-plan-could-not-predict-test
  (let [wave {:index 1
              :steps [{:project "mid" :lib 'io.github.test/mid}]
              :await {:mode :wait :timeout-ms 1000
                      :libs [{:lib 'io.github.test/mid
                              :newer-than "0.2.4" :expect nil}]}}
        outcomes [{:project "mid" :status :released :version "0.2.5"}]]
    (is (= [{:lib 'io.github.test/mid :newer-than "0.2.4" :expect "0.2.5"}]
           (:libs (exec/await-directive wave outcomes))))))

(deftest an-artifact-that-never-publishes-aborts-the-run-test
  (let [impatient (cas/plan-cascade fleet #{"weave"} {:await {:timeout-ms 0}})
        [_ result] (run (p/memory-port {:publish-after 99}) impatient {})
        {:keys [error]} result]
    (is (= :exec/await-failed (:kind error)))
    (is (= :aborted (:status (:run error))))
    (is (= 1 (count (:waves (:run error)))) "it stopped after the first wave")
    (is (= {"weave" "0.3.1"} (:released (:run error))))
    (is (str/includes? (exec/format-failure error) "await timed out"))))

;; =============================================================================
;; Unit — partial runs stay inspectable
;; =============================================================================

(deftest a-failed-step-aborts-the-run-and-keeps-the-record-test
  (let [port (p/memory-port {:fail #{"mid"}})
        [_ result] (run port)
        {:keys [error]} result]
    (is (= :exec/step-failed (:kind error)))
    (is (= ["mid"] (:failed error)))
    (is (= {"weave" "0.3.1"} (:released (:run error)))
        "the first wave stands; nothing downstream ran")
    (is (= [:released :release-failed]
           (mapv :status (mapcat :steps (:waves (:run error))))))
    (is (str/includes? (exec/format-failure error) "mid"))))

(deftest a-failed-step-still-reports-the-pins-it-wrote-test
  (let [port (p/memory-port {:fail #{"mid"}})
        [_ result] (run port)
        mid (last (mapcat :steps (:waves (:run (:error result)))))]
    (is (= ["weave"] (mapv :dep (:pin-updates mid))))))

;; =============================================================================
;; Unit — progress is emitted
;; =============================================================================

(deftest every-step-emits-a-start-and-an-end-test
  (let [events (atom [])
        _ (run (p/memory-port) plan {:emit #(swap! events conj (:type %))})
        counts (frequencies @events)]
    (is (= 3 (:wave/start counts)))
    (is (= 3 (:step/start counts)))
    (is (= 3 (:step/end counts)))))

;; =============================================================================
;; Properties
;; =============================================================================

(defn- release-order
  [port]
  (into {}
        (map-indexed (fn [i entry] [(second entry) i]))
        (filter #(= :release (first %)) (:log @(:state port)))))

(defspec a-run-releases-exactly-what-the-plan-listed 50
  (prop/for-all [[_ _ a-plan] ct/gen-plan]
    (let [port (p/memory-port)
          result (exec/run-plan! port port a-plan quiet)]
      (and (some? (:ok result))
           (= (set (cas/plan-projects a-plan))
              (set (keys (:released (:ok result)))))))))

(defspec a-run-never-releases-a-consumer-before-its-dependency 50
  (prop/for-all [[gr _ a-plan] ct/gen-plan]
    (let [port (p/memory-port)
          result (exec/run-plan! port port a-plan quiet)
          order (release-order port)]
      (and (some? (:ok result))
           (every? (fn [project]
                     (every? #(< (long (get order %)) (long (get order project)))
                             (filter order (g/depends-on gr project))))
                   (keys order))))))

(defspec no-pin-is-ever-written-with-an-unknown-target 50
  (prop/for-all [[_ _ a-plan] ct/gen-plan]
    (let [port (p/memory-port)]
      (exec/run-plan! port port a-plan quiet)
      (every? some? (mapcat vals (vals (:pins @(:state port))))))))

(defspec an-aborted-run-still-reports-every-wave-it-finished 50
  (prop/for-all [[_ _ a-plan] ct/gen-plan]
    (let [victim (first (cas/plan-projects a-plan))
          port (p/memory-port {:fail #{victim}})
          result (exec/run-plan! port port a-plan quiet)
          record (:run (:error result))]
      (and (nil? (:ok result))
           (= :aborted (:status record))
           (some (comp #{:release-failed} :status)
                 (mapcat :steps (:waves record)))))))