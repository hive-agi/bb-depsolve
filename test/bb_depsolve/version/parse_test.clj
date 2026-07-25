(ns bb-depsolve.version.parse-test
  "Unit tests for dependency extraction."
  (:require [clojure.test :refer [deftest is testing]]
            [bb-depsolve.version.api :as v]))

(deftest find-git-deps-test
  (testing "parses git deps from edn content"
    (let [content "io.github.hive-agi/hive-events {:git/tag \"v0.3.0\" :git/sha \"abc1234\"}"
          deps (v/find-git-deps content)]
      (is (= 1 (count deps)))
      (is (= 'io.github.hive-agi/hive-events (:lib (first deps))))
      (is (= "v0.3.0" (:tag (first deps))))
      (is (= "abc1234" (:sha (first deps)))))))

(deftest find-mvn-deps-test
  (testing "parses mvn deps from edn content"
    (let [content "cheshire/cheshire {:mvn/version \"5.13.0\"}"
          deps (v/find-mvn-deps content)]
      (is (= 1 (count deps)))
      (is (= 'cheshire/cheshire (:lib (first deps))))
      (is (= "5.13.0" (:version (first deps)))))))

(deftest find-shadow-deps-test
  (testing "parses qualified deps from shadow-cljs.edn :dependencies"
    (let [content ":dependencies [[re-frame/re-frame \"1.4.3\"]\n                [day8.re-frame/http-fx \"0.2.4\"]]"
          deps (v/find-shadow-deps content)]
      (is (= 2 (count deps)))
      (is (= 're-frame/re-frame (:lib (first deps))))
      (is (= "1.4.3" (:version (first deps))))
      (is (= 'day8.re-frame/http-fx (:lib (second deps))))
      (is (= "0.2.4" (:version (second deps))))))

  (testing "normalizes unqualified deps to group=artifact"
    (let [content ":dependencies [[reagent \"2.0.1\"]\n                [haslett \"0.1.6\"]]"
          deps (v/find-shadow-deps content)]
      (is (= 2 (count deps)))
      (is (= 'reagent/reagent (:lib (first deps))))
      (is (= "2.0.1" (:version (first deps))))
      (is (= 'haslett/haslett (:lib (second deps))))
      (is (= "0.1.6" (:version (second deps))))))

  (testing "handles mixed qualified and unqualified"
    (let [content ":dependencies [[re-frame \"1.4.3\"]\n                [metosin/reitit \"0.7.0-alpha7\"]\n                [binaryage/devtools \"1.0.7\"]]"
          deps (v/find-shadow-deps content)]
      (is (= 3 (count deps)))
      (is (= 're-frame/re-frame (:lib (first deps))))
      (is (= 'metosin/reitit (:lib (second deps))))
      (is (= 'binaryage/devtools (:lib (nth deps 2))))))

  (testing "handles deps with :scope metadata"
    (let [content ":dependencies [[reagent \"2.0.1\" :scope \"test\"]]"
          deps (v/find-shadow-deps content)]
      (is (= 1 (count deps)))
      (is (= 'reagent/reagent (:lib (first deps))))
      (is (= "2.0.1" (:version (first deps))))))

  (testing "returns empty vec for no dependencies"
    (let [content "{:deps true :builds {:app {:target :browser}}}"
          deps (v/find-shadow-deps content)]
      (is (empty? deps))))

  (testing "real-world olympus shadow-cljs.edn content"
    (let [content (str ":dependencies [[re-frame \"1.4.3\"]\n"
                       "                [reagent \"2.0.1\"]\n"
                       "                [day8.re-frame/http-fx \"0.2.4\"]\n"
                       "                [haslett \"0.1.6\"]\n"
                       "                [metosin/reitit \"0.7.0-alpha7\"]\n"
                       "                [binaryage/devtools \"1.0.7\"]]")
          deps (v/find-shadow-deps content)]
      (is (= 6 (count deps)))
      (is (= #{'re-frame/re-frame 'reagent/reagent 'day8.re-frame/http-fx
               'haslett/haslett 'metosin/reitit 'binaryage/devtools}
             (set (map :lib deps)))))))

(deftest find-local-deps-test
  (testing "parses :local/root deps from edn content"
    (let [content "io.github.hive-agi/hive-events {:local/root \"../hive-events\"}"
          deps (v/find-local-deps content)]
      (is (= 1 (count deps)))
      (is (= 'io.github.hive-agi/hive-events (:lib (first deps))))
      (is (= "../hive-events" (:path (first deps))))))

  (testing "finds multiple local deps"
    (let [content (str "io.github.hive-agi/hive-events {:local/root \"../hive-events\"}\n"
                       "io.github.hive-agi/hive-dsl {:local/root \"../hive-dsl\"}")
          deps (v/find-local-deps content)]
      (is (= 2 (count deps)))
      (is (= 'io.github.hive-agi/hive-events (:lib (first deps))))
      (is (= 'io.github.hive-agi/hive-dsl (:lib (second deps))))))

  (testing "returns empty vec when no local deps"
    (let [content "cheshire/cheshire {:mvn/version \"5.13.0\"}"
          deps (v/find-local-deps content)]
      (is (empty? deps))))

  (testing "handles mixed dep types"
    (let [content (str "cheshire/cheshire {:mvn/version \"5.13.0\"}\n"
                       "io.github.hive-agi/hive-events {:local/root \"../hive-events\"}\n"
                       "io.github.hive-agi/hive-dsl {:git/tag \"v0.3.0\" :git/sha \"abc1234\"}")
          deps (v/find-local-deps content)]
      (is (= 1 (count deps)))
      (is (= 'io.github.hive-agi/hive-events (:lib (first deps)))))))

(deftest deps-edn->dep-coords-test
  (testing "extracts mvn deps"
    (let [edn-str "{:deps {cheshire/cheshire {:mvn/version \"6.1.0\"}}}"
          deps (v/deps-edn->dep-coords edn-str)]
      (is (= 1 (count deps)))
      (is (= 'cheshire/cheshire (:lib (first deps))))
      (is (= "6.1.0" (:version (first deps))))
      (is (= :mvn (:type (first deps))))))

  (testing "extracts git deps"
    (let [edn-str "{:deps {io.github.hive-agi/hive-dsl {:git/tag \"v0.3.7\" :git/sha \"abc\"}}}"
          deps (v/deps-edn->dep-coords edn-str)]
      (is (= 1 (count deps)))
      (is (= 'io.github.hive-agi/hive-dsl (:lib (first deps))))
      (is (= "v0.3.7" (:version (first deps))))
      (is (= :git (:type (first deps))))))

  (testing "skips local/root deps"
    (let [edn-str "{:deps {foo/bar {:local/root \"../bar\"}}}"
          deps (v/deps-edn->dep-coords edn-str)]
      (is (= 0 (count deps)))))

  (testing "returns empty for nil"
    (is (= [] (v/deps-edn->dep-coords nil)))))

(deftest parse-tag-output-prefers-peeled-commit-test
  (testing "remote annotated tag uses peeled commit even when tag object appears first"
    (is (= [{:tag "v0.1.0" :sha "1111111111111111111111111111111111111111"
             :sha-short "1111111"}
            {:tag "v0.2.0" :sha "3333333333333333333333333333333333333333"
             :sha-short "3333333"}]
           (v/parse-ls-remote-tags
            (str "1111111111111111111111111111111111111111\trefs/tags/v0.1.0\n"
                 "2222222222222222222222222222222222222222\trefs/tags/v0.2.0\n"
                 "3333333333333333333333333333333333333333\trefs/tags/v0.2.0^{}\n")))))
  (testing "remote peeled line keeps precedence when it appears first"
    (is (= "3333333333333333333333333333333333333333"
           (:sha (first (v/parse-ls-remote-tags
                         (str "3333333333333333333333333333333333333333\trefs/tags/v0.2.0^{}\n"
                              "2222222222222222222222222222222222222222\trefs/tags/v0.2.0\n")))))))
  (testing "local output chooses peeled SHA for annotated and direct SHA for lightweight tags"
    (is (= [{:tag "v0.2.0" :sha "3333333333333333333333333333333333333333"
             :sha-short "3333333"}
            {:tag "v0.1.0" :sha "1111111111111111111111111111111111111111"
             :sha-short "1111111"}]
           (v/parse-local-tag-output
            (str "v0.2.0\t3333333333333333333333333333333333333333\t2222222222222222222222222222222222222222\n"
                 "v0.1.0\t\t1111111111111111111111111111111111111111\n")))))
  (testing "malformed lines are ignored"
    (is (= [] (v/parse-local-tag-output "")))
    (is (= [] (v/parse-ls-remote-tags nil)))))
