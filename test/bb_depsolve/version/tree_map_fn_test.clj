(ns bb-depsolve.version.tree-map-fn-test
  "build-dep-tree's :map-fn seam: an injected fan-out must give the same tree
   as the serial default, and a failed lookup must stay visible in place.
   Kanban 20260725145720-00afadf1."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [bb-depsolve.core.tree :as tree-cmd]
            [bb-depsolve.version.tree :as tree]))

(def ^:private graph
  "A small dependency graph with a diamond and a cycle."
  {'a/a [{:lib 'b/b :version "1" :type :mvn} {:lib 'c/c :version "1" :type :mvn}]
   'b/b [{:lib 'd/d :version "1" :type :mvn}]
   'c/c [{:lib 'd/d :version "1" :type :mvn} {:lib 'a/a :version "1" :type :mvn}]
   'd/d []})

(defn- resolve-fn [lib _version] (get graph lib []))

(defn parallel-equals-serial
  "[roots depth] -> true when the bounded fan-out tree equals the serial one."
  [[roots depth]]
  (= (tree/build-dep-tree roots resolve-fn depth)
     (tree/build-dep-tree roots resolve-fn depth {:map-fn tree-cmd/fetch-level})))

(def ^:private gen-root
  (gen/fmap (fn [lib] {:lib lib :version "1" :type :mvn})
            (gen/elements (keys graph))))

(deftrifecta fetch-level-preserves-the-tree
  bb-depsolve.version.tree-map-fn-test/parallel-equals-serial
  {:golden-path "test/golden/version/tree-map-fn.edn"
   :cases {:diamond-cycle [[{:lib 'a/a :version "1" :type :mvn}] 4]
           :leaf          [[{:lib 'd/d :version "1" :type :mvn}] 2]
           :depth-zero    [[{:lib 'a/a :version "1" :type :mvn}] 0]
           :empty         [[] 3]}
   :gen (gen/tuple (gen/vector gen-root 0 4) (gen/choose 0 5))
   :pred true?
   :num-tests 50
   :mutations [["always-false" (fn [_] false)]]})

(deftest a-failed-lookup-is-kept-as-unresolved
  (testing "a nil from map-fn keeps the dep, marked :unresolved?, not dropped"
    (let [drop-b (fn [f coll] (mapv #(when-not (= 'b/b (:lib %)) (f %)) coll))
          t      (tree/build-dep-tree [{:lib 'a/a :version "1" :type :mvn}]
                                      resolve-fn 3 {:map-fn drop-b})
          b      (first (:children (first t)))]
      (is (= 'b/b (:lib b)))
      (is (:unresolved? b))
      (is (= [] (:children b)))
      (is (= [['b/b "1"]] (tree-cmd/unresolved-libs t))))))

(deftest a-throwing-lookup-surfaces-through-fetch-level
  (testing "bounded-pmap turns a throw into nil, which the tree reports"
    (let [boom (fn [lib v] (if (= 'c/c lib) (throw (ex-info "boom" {})) (resolve-fn lib v)))
          t    (tree/build-dep-tree [{:lib 'a/a :version "1" :type :mvn}]
                                    boom 3 {:map-fn tree-cmd/fetch-level})]
      (is (= [['c/c "1"]] (tree-cmd/unresolved-libs t)))
      (is (= 2 (count (:children (first t))))))))
