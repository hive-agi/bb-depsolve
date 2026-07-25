(ns bb-depsolve.graph-test
  "Unit and property tests for bb-depsolve.graph.dag (pure Calculation layer).

   The properties are the ordering contract the cascade planner relies on:
   waves are topologically sound, they partition the graph with the cyclic
   residue, and the downstream closure is upward-closed."
  (:require [bb-depsolve.graph.dag :as g]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- node
  ([p] (node p :pinned "0.1.0"))
  ([p mode version]
   {:project p :lib (symbol "io.github.test" p) :dir p
    :release-mode mode :version version}))

(defn- pin
  ([from to] (pin from to :runtime))
  ([from to scope]
   {:project from :dep to :lib (symbol "io.github.test" to)
    :coord :mvn :version "0.1.0" :path (str from "/deps.edn") :scope scope}))

(def ^:private chain
  "spi <- dsl <- weave <- carto, plus an unrelated leaf."
  (g/dep-graph (map node ["spi" "dsl" "weave" "carto" "loner"])
               [(pin "dsl" "spi") (pin "weave" "dsl")
                (pin "carto" "weave") (pin "carto" "spi")]))

;; =============================================================================
;; Unit — structure
;; =============================================================================

(deftest dep-graph-drops-unknown-endpoints-test
  (let [gr (g/dep-graph [(node "a")] [(pin "a" "ghost") (pin "ghost" "a")])]
    (is (= ["a"] (g/projects gr)))
    (is (= #{} (g/depends-on gr "a")))
    (is (empty? (:pins gr)))))

(deftest dep-graph-indexes-every-pin-per-edge-test
  (let [gr (g/dep-graph [(node "a") (node "b")]
                        [(pin "a" "b")
                         (assoc (pin "a" "b") :path "a/bb.edn" :coord :git)])]
    (is (= 2 (count (get-in gr [:pins ["a" "b"]])))
        "both dep files are recorded under the single edge")))

(deftest edge-predicate-separates-ordering-from-rewriting-test
  (let [gr (g/dep-graph [(node "a") (node "b")]
                        [(pin "a" "b" :alias)]
                        {:edge? #(= :runtime (:scope %))})]
    (is (= #{} (g/depends-on gr "a"))
        "an alias-scoped pin does not constrain release order")
    (is (seq (get-in gr [:pins ["a" "b"]]))
        "but it is still indexed, so it can be rewritten")))

;; =============================================================================
;; Unit — ordering
;; =============================================================================

(deftest waves-order-dependencies-first-test
  (let [{:keys [waves cyclic]} (g/waves chain)]
    (is (empty? cyclic))
    (is (= [["loner" "spi"] ["dsl"] ["weave"] ["carto"]] waves))))

(deftest topo-order-flattens-waves-test
  (is (= ["loner" "spi" "dsl" "weave" "carto"] (:order (g/topo-order chain)))))

(deftest reverse-edges-and-dependents-test
  (is (= #{"carto" "dsl"} (g/dependents chain "spi")))
  (is (= #{} (g/dependents chain "carto"))))

(deftest downstream-closure-includes-seeds-test
  (is (= #{"dsl" "weave" "carto"} (g/downstream-closure chain #{"dsl"})))
  (is (= #{"carto"} (g/downstream-closure chain #{"carto"})))
  (is (= #{"absent"} (g/downstream-closure chain #{"absent"}))
      "an unknown seed is retained rather than silently dropped"))

(deftest induced-subgraph-restricts-nodes-edges-and-pins-test
  (let [sub (g/induced-subgraph chain #{"dsl" "weave"})]
    (is (= ["dsl" "weave"] (g/projects sub)))
    (is (= #{} (g/depends-on sub "dsl")) "the dropped spi edge is gone")
    (is (= #{"dsl"} (g/depends-on sub "weave")))
    (is (= [["weave" "dsl"]] (keys (:pins sub)))
        "only the consumer/dependency pair whose endpoints both survive")))

;; =============================================================================
;; Unit — cycles
;; =============================================================================

(deftest cycles-distinguish-members-from-blocked-test
  (let [gr (g/dep-graph (map node ["a" "b" "c" "d"])
                        [(pin "a" "b") (pin "b" "a") (pin "c" "a") (pin "d" "d")])]
    (is (= [#{"a" "b"} #{"d"}] (g/cycles gr))
        "mutual pair and self-loop are both cycles")
    (is (= #{"c"} (g/blocked gr))
        "c is unorderable but is not itself on a cycle")
    (is (= #{"a" "b" "c" "d"} (:cyclic (g/waves gr))))
    (is (empty? (:waves (g/waves gr))))))

(deftest acyclic-graph-reports-no-cycles-test
  (is (empty? (g/cycles chain)))
  (is (empty? (g/blocked chain))))

;; =============================================================================
;; Generators
;; =============================================================================

(def gen-dag
  "A random acyclic graph: project i may only depend on projects j < i."
  (gen/let [n (gen/choose 1 8)]
    (let [names (mapv #(str "p" %) (range n))]
      (gen/let [dep-idxs (apply gen/tuple
                                (for [i (range n)]
                                  (if (zero? i)
                                    (gen/return [])
                                    (gen/vector-distinct (gen/choose 0 (dec i))
                                                         {:max-elements i}))))]
        (g/dep-graph (map node names)
                     (for [i (range n)
                           j (nth dep-idxs i)]
                       (pin (names i) (names j))))))))

(def gen-dag+seeds
  (gen/let [gr gen-dag
            seeds (gen/not-empty
                   (gen/vector-distinct (gen/elements (g/projects gr))))]
    [gr (set seeds)]))

;; =============================================================================
;; Properties
;; =============================================================================

(defspec waves-are-topologically-sound 100
  (prop/for-all [gr gen-dag]
    (let [{:keys [waves]} (g/waves gr)
          level (into {} (for [[i w] (map-indexed vector waves), p w] [p i]))]
      (every? (fn [p]
                (every? #(< (long (get level %)) (long (get level p)))
                        (g/depends-on gr p)))
              (keys level)))))

(defspec waves-and-cyclic-partition-the-graph 100
  (prop/for-all [gr gen-dag]
    (let [{:keys [waves cyclic]} (g/waves gr)
          ordered (into [] cat waves)]
      (and (= (count ordered) (count (set ordered)))
           (empty? (set/intersection (set ordered) cyclic))
           (= (set (g/projects gr)) (into (set ordered) cyclic))))))

(defspec an-acyclic-graph-orders-completely 100
  (prop/for-all [gr gen-dag]
    (and (empty? (:cyclic (g/waves gr)))
         (empty? (g/cycles gr))
         (empty? (g/blocked gr)))))

(defspec closure-is-upward-closed 100
  (prop/for-all [[gr seeds] gen-dag+seeds]
    (let [closure (g/downstream-closure gr seeds)]
      (and (set/subset? seeds closure)
           (every? (fn [p] (set/subset? (g/dependents gr p) closure)) closure)))))

(defspec closure-subgraph-keeps-its-own-order 100
  (prop/for-all [[gr seeds] gen-dag+seeds]
    (let [sub (g/induced-subgraph gr (g/downstream-closure gr seeds))
          {:keys [waves]} (g/waves sub)
          level (into {} (for [[i w] (map-indexed vector waves), p w] [p i]))]
      (every? (fn [p]
                (every? #(< (long (get level %)) (long (get level p)))
                        (g/depends-on sub p)))
              (keys level)))))

(defspec every-cycle-member-reaches-itself 100
  (prop/for-all [gr gen-dag]
    (testing "the DAG generator never produces a cycle"
      (empty? (mapcat identity (g/cycles gr))))))
