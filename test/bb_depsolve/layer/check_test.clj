(ns bb-depsolve.layer.check-test
  "The layer check decides whether a dependency edge is legal, so a wrong
   verdict either blocks a valid pin or lets a layering defect ship."
  (:require [bb-depsolve.layer.check :as check]
            [bb-depsolve.layer.rules :as rules]
            [bb-depsolve.layer.table :as table]
            [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def raw-table
  {:layers  [{:name :primitives :projects ["dsl"]}
             {:name :contracts  :projects ["spi" "contracts"]}
             {:name :impl       :projects ["store" "index"]}
             {:name :apps       :terminal? true :projects ["server" "cli"]}]
   :waivers [{:from "index" :to "server" :reason "legacy, scheduled for removal"}]})

(def tbl (table/parse raw-table))

;; =============================================================================
;; Table indexing
;; =============================================================================

(deftest parse-indexes-projects-to-their-level-test
  (is (= 0 (table/level-of tbl "dsl")))
  (is (= 1 (table/level-of tbl "spi")))
  (is (= 3 (table/level-of tbl "server")))
  (is (nil? (table/level-of tbl "unknown-project"))
      "a project absent from the table has no level"))

(deftest parse-records-terminal-levels-test
  (is (= #{3} (:terminal tbl)))
  (is (= [:primitives :contracts :impl :apps] (:names tbl))))

(deftest level-name-resolves-through-the-ordered-names-test
  (is (= :contracts (table/level-name tbl 1)))
  (is (nil? (table/level-name tbl nil))))

(deftest unranked-lists-only-projects-the-table-omits-test
  (is (= ["mystery" "other"]
         (table/unranked tbl ["dsl" "other" "spi" "mystery"]))))

;; =============================================================================
;; Edge classification
;; =============================================================================

(deftest downward-edges-are-the-point-test
  (is (= :ok (:verdict (check/classify-edge tbl "store" "dsl"))))
  (is (= :ok (:verdict (check/classify-edge tbl "spi" "dsl")))))

(deftest sideways-edges-are-allowed-test
  (is (= :sideways (:verdict (check/classify-edge tbl "contracts" "spi")))
      "one contract may use another"))

(deftest upward-edges-are-violations-test
  (is (= :violation (:verdict (check/classify-edge tbl "dsl" "spi")))
      "a primitive must not reach up into a contract"))

(deftest nothing-may-depend-on-a-terminal-layer-test
  (testing "even a downward edge into an application is a violation"
    (is (= :violation (:verdict (check/classify-edge tbl "cli" "server")))
        "sideways within a terminal layer still counts")
    (is (= :violation (:verdict (check/classify-edge tbl "store" "server")))
        "an implementation must not pin an application")))

(deftest a-waiver-beats-the-terminal-rule-test
  (let [{:keys [verdict reason]} (check/classify-edge tbl "index" "server")]
    (is (= :waived verdict))
    (is (= "legacy, scheduled for removal" reason)
        "the waiver reason travels with the classified edge")))

(deftest unranked-projects-are-exempt-test
  (is (= :unranked (:verdict (check/classify-edge tbl "mystery" "server")))
      "an unranked consumer cannot be judged")
  (is (= :unranked (:verdict (check/classify-edge tbl "store" "mystery")))
      "an unranked dependency cannot be judged"))

;; =============================================================================
;; Whole-graph check
;; =============================================================================

(def fixture-edges
  {"server"   #{"store" "index" "spi" "dsl"}
   "store"    #{"dsl" "spi"}
   "index"    #{"dsl" "server"}
   "cli"      #{"server"}
   "stranger" #{"dsl"}})

(deftest check-separates-violations-from-legal-edges-test
  (let [{:keys [violations summary unranked]} (check/check tbl fixture-edges)]
    (is (= [["cli" "server"]] (mapv (juxt :from :to) violations))
        "only the un-waived terminal edge is a violation")
    (is (= 1 (:waived summary)) "index->server is waived, not counted")
    (is (= ["stranger"] unranked))))

(deftest check-counts-every-edge-exactly-once-test
  (let [{:keys [classified summary]} (check/check tbl fixture-edges)
        edge-count (reduce + (map count (vals fixture-edges)))]
    (is (= edge-count (count classified)))
    (is (= edge-count (reduce + (vals summary))))))

;; =============================================================================
;; Rule chain is extensible without editing it
;; =============================================================================

(deftest prepending-a-rule-overrides-the-defaults-test
  (let [permissive (into [{:name :allow-everything-from-cli
                           :when #(= "cli" (:from %))
                           :verdict :ok}]
                         rules/default-layer-rules)]
    (is (= :violation (:verdict (check/classify-edge tbl "cli" "server")))
        "default chain still rejects it")
    (is (= :ok (:verdict (check/classify-edge permissive tbl "cli" "server")))
        "the prepended rule decides first")))

(deftest every-verdict-the-chain-emits-is-declared-test
  (let [{:keys [classified]} (check/check tbl fixture-edges)]
    (is (every? rules/verdicts (map :verdict classified)))))
