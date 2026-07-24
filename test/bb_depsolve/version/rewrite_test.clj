(ns bb-depsolve.version.rewrite-test
  "Unit tests for dependency rewriting."
  (:require [clojure.test :refer [deftest is testing]]
            [bb-depsolve.version :as v]
            [clojure.string :as str]))

(deftest update-shadow-dep-test
  (testing "updates qualified dep version"
    (let [content ":dependencies [[day8.re-frame/http-fx \"0.2.4\"]]"
          updated (v/update-shadow-dep content 'day8.re-frame/http-fx "0.3.0")]
      (is (= ":dependencies [[day8.re-frame/http-fx \"0.3.0\"]]" updated))))

  (testing "updates unqualified dep version (lib sym has group=artifact)"
    (let [content ":dependencies [[reagent \"2.0.1\"]]"
          updated (v/update-shadow-dep content 'reagent/reagent "2.1.0")]
      (is (= ":dependencies [[reagent \"2.1.0\"]]" updated))))

  (testing "preserves other deps when updating one"
    (let [content (str ":dependencies [[re-frame \"1.4.3\"]\n"
                       "                [reagent \"2.0.1\"]]")
          updated (v/update-shadow-dep content 'reagent/reagent "2.1.0")]
      (is (str/includes? updated "[re-frame \"1.4.3\"]"))
      (is (str/includes? updated "[reagent \"2.1.0\"]"))))

  (testing "updates only the target dep in multi-dep list"
    (let [content (str ":dependencies [[re-frame \"1.4.3\"]\n"
                       "                [day8.re-frame/http-fx \"0.2.4\"]\n"
                       "                [reagent \"2.0.1\"]]")
          updated (v/update-shadow-dep content 'day8.re-frame/http-fx "0.3.0")]
      (is (str/includes? updated "[re-frame \"1.4.3\"]"))
      (is (str/includes? updated "[day8.re-frame/http-fx \"0.3.0\"]"))
      (is (str/includes? updated "[reagent \"2.0.1\"]")))))

(deftest update-git-dep-test
  (testing "replaces tag and sha in content"
    (let [content "io.github.hive-agi/hive-events {:git/tag \"v0.3.0\" :git/sha \"abc1234\"}"
          updated (v/update-git-dep content 'io.github.hive-agi/hive-events "v0.4.0" "def5678")]
      (is (= "io.github.hive-agi/hive-events {:git/tag \"v0.4.0\" :git/sha \"def5678\"}" updated)))))

(deftest update-mvn-dep-test
  (testing "replaces version in content"
    (let [content "cheshire/cheshire {:mvn/version \"5.13.0\"}"
          updated (v/update-mvn-dep content 'cheshire/cheshire "5.14.0")]
      (is (= "cheshire/cheshire {:mvn/version \"5.14.0\"}" updated)))))

(deftest sha-matches?-test
  (testing "prefix match"
    (is (true? (v/sha-matches? "abc1234" "abc1234567890")))
    (is (true? (v/sha-matches? "abc1234567890" "abc1234"))))
  (testing "mismatch"
    (is (false? (v/sha-matches? "abc" "def"))))
  (testing "nil safety"
    (is (nil? (v/sha-matches? nil "abc")))))

(deftest replace-local-with-git-test
  (testing "replaces :local/root with :git/tag+sha"
    (let [content "io.github.hive-agi/hive-events {:local/root \"../hive-events\"}"
          updated (v/replace-local-with-git content 'io.github.hive-agi/hive-events "v0.4.0" "def5678")]
      (is (= "io.github.hive-agi/hive-events {:git/tag \"v0.4.0\" :git/sha \"def5678\"}" updated))))

  (testing "preserves other deps"
    (let [content (str "cheshire/cheshire {:mvn/version \"5.13.0\"}\n"
                       "io.github.hive-agi/hive-events {:local/root \"../hive-events\"}")
          updated (v/replace-local-with-git content 'io.github.hive-agi/hive-events "v0.4.0" "def5678")]
      (is (str/includes? updated "cheshire/cheshire {:mvn/version \"5.13.0\"}"))
      (is (str/includes? updated "{:git/tag \"v0.4.0\" :git/sha \"def5678\"}")))))

(deftest replace-local-with-mvn-test
  (testing "replaces :local/root with :mvn/version"
    (let [content "cheshire/cheshire {:local/root \"../cheshire\"}"
          updated (v/replace-local-with-mvn content 'cheshire/cheshire "5.14.0")]
      (is (= "cheshire/cheshire {:mvn/version \"5.14.0\"}" updated)))))

(deftest sync-changes-in-content-test
  (let [resolved '{io.github.hive-agi/hive-test {:tag "v0.3.6"
                                                 :sha "2fe7a14aaaaaaa"
                                                 :sha-short "2fe7a14"}
                   io.github.hive-agi/hive-mcp  {:tag "v0.18.0"
                                                 :sha "088c7dfbbbbbbb"
                                                 :sha-short "088c7df"}}]

    (testing "git drift detected (tag and sha)"
      (is (= [{:lib 'io.github.hive-agi/hive-test :coord :git
               :old-tag "v0.3.0" :old-sha "763e4bc"
               :new-tag "v0.3.6" :new-sha "2fe7a14"}]
             (v/sync-changes-in-content
              "{:deps {io.github.hive-agi/hive-test {:git/tag \"v0.3.0\" :git/sha \"763e4bc\"}}}"
              resolved))))

    (testing "mvn drift detected against tag->mvn-version"
      (is (= [{:lib 'io.github.hive-agi/hive-test :coord :mvn
               :old-version "0.3.0" :new-version "0.3.6"}]
             (v/sync-changes-in-content
              "{:deps {io.github.hive-agi/hive-test {:mvn/version \"0.3.0\"}}}"
              resolved))))

    (testing "in-sync coords produce no changes"
      (is (= [] (v/sync-changes-in-content
                 "{:deps {io.github.hive-agi/hive-test {:mvn/version \"0.3.6\"}
                          io.github.hive-agi/hive-mcp {:git/tag \"v0.18.0\" :git/sha \"088c7df\"}}}"
                 resolved))))

    (testing "unresolved libs are ignored"
      (is (= [] (v/sync-changes-in-content
                 "{:deps {cheshire/cheshire {:mvn/version \"5.13.0\"}}}"
                 resolved))))

    (testing "mixed git and mvn drift in one file"
      (is (= #{{:lib 'io.github.hive-agi/hive-test :coord :mvn
                :old-version "0.2.1" :new-version "0.3.6"}
               {:lib 'io.github.hive-agi/hive-mcp :coord :git
                :old-tag "v0.16.7" :old-sha "0222203"
                :new-tag "v0.18.0" :new-sha "088c7df"}}
             (set (v/sync-changes-in-content
                   "{:deps {io.github.hive-agi/hive-mcp {:git/tag \"v0.16.7\" :git/sha \"0222203\"}
                            io.github.hive-agi/hive-test {:mvn/version \"0.2.1\"}}}"
                   resolved)))))))
