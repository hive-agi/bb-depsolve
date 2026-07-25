(ns bb-depsolve.layer.freeze-test
  "A frozen layer's whole point is that consumers can pin it and forget it,
   so the cadence budget has to count the right releases in the right window."
  (:require [bb-depsolve.layer.freeze :as freeze]
            [bb-depsolve.layer.table :as table]
            [clojure.test :refer [deftest is testing]]))

(def ^:private now 1000000000)
(defn- days-ago [n] (- now (* n 86400)))

;; =============================================================================
;; Window arithmetic
;; =============================================================================

(deftest releases-within-counts-only-the-window-test
  (let [ts [(days-ago 1) (days-ago 30) (days-ago 89) (days-ago 91) (days-ago 400)]]
    (is (= 3 (freeze/releases-within ts now 90)))
    (is (= 2 (freeze/releases-within ts now 45)))
    (is (= 5 (freeze/releases-within ts now 3650)))))

(deftest a-release-exactly-at-the-boundary-is-outside-test
  (is (= 0 (freeze/releases-within [(days-ago 90)] now 90))
      "the cutoff is exclusive, so a tag exactly 90 days old has aged out"))

(deftest no-tags-means-no-releases-test
  (is (= 0 (freeze/releases-within [] now 90))))

;; =============================================================================
;; Budget
;; =============================================================================

(deftest over-budget-is-strict-test
  (is (false? (freeze/over-budget? 4 4)) "at budget is not over budget")
  (is (true? (freeze/over-budget? 5 4)))
  (is (false? (freeze/over-budget? 0 4))))

(deftest a-nil-budget-is-unlimited-test
  (is (false? (freeze/over-budget? 9999 nil))
      "an unfrozen layer must never be reported"))

;; =============================================================================
;; Which projects are frozen
;; =============================================================================

(def ^:private tbl
  (table/parse
   {:layers [{:name :contracts :frozen? true :max-releases 2
              :projects ["spi" "contracts"]}
             {:name :util :projects ["dsl"]}
             {:name :apps :terminal? true :projects ["app"]}]}))

(deftest only-frozen-layers-carry-a-budget-test
  (is (= [{:project "contracts" :level 0 :budget 2}
          {:project "spi" :level 0 :budget 2}]
         (freeze/frozen-projects tbl)))
  (is (= {0 2} (:frozen tbl)) "the budget is indexed by level"))

(deftest frozen?-without-max-releases-takes-the-default-test
  (let [t (table/parse {:layers [{:name :c :frozen? true :projects ["x"]}]})]
    (is (= {0 table/default-freeze-budget} (:frozen t)))))

(deftest max-releases-alone-freezes-a-layer-test
  (let [t (table/parse {:layers [{:name :c :max-releases 1 :projects ["x"]}]})]
    (is (= {0 1} (:frozen t))
        "an explicit budget implies the freeze; :frozen? is sugar")))

(deftest an-unfrozen-table-has-no-frozen-projects-test
  (let [t (table/parse {:layers [{:name :a :projects ["x"]}]})]
    (is (empty? (freeze/frozen-projects t)))))

;; =============================================================================
;; Report shape
;; =============================================================================

(deftest breaches-selects-only-over-budget-entries-test
  (let [report [{:project "a" :over? true} {:project "b" :over? false}
                {:project "c" :over? true}]]
    (is (= ["a" "c"] (mapv :project (freeze/breaches report))))))

(deftest check-skips-projects-absent-from-the-graph-test
  (testing "a table may name a project the workspace does not contain"
    (is (empty? (freeze/check tbl {} now 90))
        "no node, no directory to read tags from, no report row")))
