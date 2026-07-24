(ns bb-depsolve.version.tree-test
  "Unit tests for dependency trees."
  (:require [clojure.test :refer [deftest is testing]]
            [bb-depsolve.version :as v]
            [clojure.string :as str]))

(deftest build-dep-tree-test
  (testing "builds tree with mock resolver"
    (let [resolve-fn (fn [lib _version]
                       (case (str lib)
                         "a/a" [{:lib 'b/b :version "1.0" :type :mvn}]
                         "b/b" [{:lib 'c/c :version "2.0" :type :mvn}]
                         []))
          deps [{:lib 'a/a :version "1.0" :type :mvn}]
          tree (v/build-dep-tree deps resolve-fn 3)]
      (is (= 1 (count tree)))
      (is (= 'a/a (:lib (first tree))))
      (is (= 1 (count (:children (first tree)))))
      (is (= 'b/b (:lib (first (:children (first tree))))))
      (is (= 1 (count (:children (first (:children (first tree)))))))))

  (testing "respects max depth"
    (let [resolve-fn (fn [_ _] [{:lib 'deep/dep :version "1.0" :type :mvn}])
          deps [{:lib 'a/a :version "1.0" :type :mvn}]
          tree (v/build-dep-tree deps resolve-fn 1)]
      (is (= 1 (count (:children (first tree)))))
      (is (= [] (:children (first (:children (first tree))))))))

  (testing "detects cycles"
    (let [resolve-fn (fn [lib _]
                       (case (str lib)
                         "a/a" [{:lib 'b/b :version "1.0" :type :mvn}]
                         "b/b" [{:lib 'a/a :version "1.0" :type :mvn}]
                         []))
          deps [{:lib 'a/a :version "1.0" :type :mvn}]
          tree (v/build-dep-tree deps resolve-fn 5)]
      (let [b-node (first (:children (first tree)))
            a-cycle (first (:children b-node))]
        (is (= 'a/a (:lib a-cycle)))
        (is (true? (:cycle? a-cycle)))
        (is (= [] (:children a-cycle)))))))

(deftest find-conflicts-test
  (testing "finds version conflicts"
    (let [tree [{:lib 'a/a :version "1.0" :children
                 [{:lib 'c/c :version "1.0" :children []}]}
                {:lib 'b/b :version "2.0" :children
                 [{:lib 'c/c :version "2.0" :children []}]}]
          conflicts (v/find-conflicts tree)]
      (is (= 1 (count conflicts)))
      (is (= #{"1.0" "2.0"} (get conflicts 'c/c)))))

  (testing "no conflicts when versions agree"
    (let [tree [{:lib 'a/a :version "1.0" :children
                 [{:lib 'c/c :version "1.0" :children []}]}
                {:lib 'b/b :version "2.0" :children
                 [{:lib 'c/c :version "1.0" :children []}]}]
          conflicts (v/find-conflicts tree)]
      (is (empty? conflicts))))

  (testing "empty tree has no conflicts"
    (is (empty? (v/find-conflicts [])))))

(deftest format-dep-tree-test
  (testing "formats simple tree"
    (let [tree [{:lib 'a/a :version "1.0" :children
                 [{:lib 'b/b :version "2.0" :children [] :cycle? false}]
                 :cycle? false}]
          lines (v/format-dep-tree tree {})]
      (is (= 2 (count lines)))
      (is (str/includes? (first lines) "a/a"))
      (is (str/includes? (second lines) "b/b"))))

  (testing "marks cycles"
    (let [tree [{:lib 'a/a :version "1.0" :cycle? true :children []}]
          lines (v/format-dep-tree tree {})]
      (is (= 1 (count lines)))
      (is (str/includes? (first lines) "cycle")))))

(deftest resolve-versions-test
  (testing "single direct dep -- trivial resolution"
    (let [tree [{:lib 'a/a :version "1.0" :type :mvn :cycle? false :children []}]
          {:keys [resolved conflicts missing]} (v/resolve-versions tree)]
      (is (= 1 (count resolved)))
      (is (= "1.0" (get-in resolved ['a/a :version])))
      (is (= 0 (get-in resolved ['a/a :depth])))
      (is (empty? conflicts))
      (is (empty? missing))))

  (testing "diamond dep -- highest version wins among same-depth ties"
    (let [tree [{:lib 'a/a :version "1.0" :type :mvn :cycle? false
                 :children [{:lib 'c/c :version "1.0" :type :mvn :cycle? false :children []}]}
                {:lib 'b/b :version "2.0" :type :mvn :cycle? false
                 :children [{:lib 'c/c :version "2.0" :type :mvn :cycle? false :children []}]}]
          {:keys [resolved conflicts]} (v/resolve-versions tree)]
      (is (= "2.0" (get-in resolved ['c/c :version])) "highest version among ties wins")
      (is (= #{"1.0" "2.0"} (get conflicts 'c/c)))))

  (testing "nearest-wins -- shallow root beats deeper transitive"
    (let [tree [{:lib 'a/a :version "1.0" :type :mvn :cycle? false
                 :children [{:lib 'shared/s :version "3.0" :type :mvn :cycle? false :children []}]}
                {:lib 'shared/s :version "1.0" :type :mvn :cycle? false :children []}]
          {:keys [resolved conflicts]} (v/resolve-versions tree)]
      (is (= "1.0" (get-in resolved ['shared/s :version])) "depth-0 wins over depth-1")
      (is (= 0 (get-in resolved ['shared/s :depth])))
      (is (= #{"1.0" "3.0"} (get conflicts 'shared/s)))))

  (testing "cycle -- cycle node not infinitely recursed, lib still resolved at root"
    (let [tree [{:lib 'a/a :version "1.0" :type :mvn :cycle? false
                 :children [{:lib 'b/b :version "1.0" :type :mvn :cycle? false
                             :children [{:lib 'a/a :version "1.0" :type :mvn :cycle? true :children []}]}]}]
          {:keys [resolved missing conflicts]} (v/resolve-versions tree)]
      (is (= "1.0" (get-in resolved ['a/a :version])))
      (is (= "1.0" (get-in resolved ['b/b :version])))
      (is (empty? missing) "a/a IS resolved at root, so not :missing")
      (is (empty? conflicts))))

  (testing "missing dep -- lib only inside a cycle, never resolved elsewhere"
    (let [tree [{:lib 'root/r :version "1.0" :type :mvn :cycle? false
                 :children [{:lib 'orphan/o :version "1.0" :type :mvn :cycle? true :children []}]}]
          {:keys [resolved missing]} (v/resolve-versions tree)]
      (is (contains? resolved 'root/r))
      (is (not (contains? resolved 'orphan/o)) "cycle-only libs not in :resolved")
      (is (= #{'orphan/o} missing))))

  (testing "no conflicts when all versions agree"
    (let [tree [{:lib 'a/a :version "1.0" :type :mvn :cycle? false
                 :children [{:lib 'c/c :version "1.0" :type :mvn :cycle? false :children []}]}
                {:lib 'b/b :version "2.0" :type :mvn :cycle? false
                 :children [{:lib 'c/c :version "1.0" :type :mvn :cycle? false :children []}]}]
          {:keys [conflicts]} (v/resolve-versions tree)]
      (is (empty? conflicts))))

  (testing "empty tree -- total / no NPE"
    (let [{:keys [resolved conflicts occurrences missing]} (v/resolve-versions [])]
      (is (= {} resolved))
      (is (= {} conflicts))
      (is (= {} occurrences))
      (is (= #{} missing))))

  (testing "occurrences preserved for downstream tooling"
    (let [tree [{:lib 'a/a :version "1.0" :type :mvn :cycle? false
                 :children [{:lib 'c/c :version "1.0" :type :mvn :cycle? false :children []}]}
                {:lib 'b/b :version "2.0" :type :mvn :cycle? false
                 :children [{:lib 'c/c :version "2.0" :type :mvn :cycle? false :children []}]}]
          {:keys [occurrences]} (v/resolve-versions tree)]
      (is (= 2 (count (get occurrences 'c/c))))
      (is (= #{"1.0" "2.0"} (set (map :version (get occurrences 'c/c))))))))
