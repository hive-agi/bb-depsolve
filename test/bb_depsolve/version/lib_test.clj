(ns bb-depsolve.version.lib-test
  "Unit tests for lib identity and coordinate naming."
  (:require [clojure.test :refer [deftest is testing]]
            [bb-depsolve.version :as v]))

(deftest parse-github-lib-test
  (testing "parses github lib coords"
    (is (= {:org "hive-agi" :repo "hive-events"}
           (v/parse-github-lib 'io.github.hive-agi/hive-events))))
  (testing "returns nil for non-github"
    (is (nil? (v/parse-github-lib 'cheshire/cheshire)))))

(deftest lib-matches-org?-test
  (is (true? (v/lib-matches-org? "hive-agi" 'io.github.hive-agi/hive-events)))
  (is (false? (v/lib-matches-org? "hive-agi" 'cheshire/cheshire))))

(deftest lib-artifact-id-test
  (is (= "hive-events" (v/lib-artifact-id 'io.github.hive-agi/hive-events)))
  (is (= "cheshire" (v/lib-artifact-id 'cheshire/cheshire))))

(deftest group-id->path-test
  (testing "converts dots to slashes"
    (is (= "com/fasterxml/jackson/core" (v/group-id->path "com.fasterxml.jackson.core"))))
  (testing "simple group"
    (is (= "cheshire" (v/group-id->path "cheshire"))))
  (testing "nil returns empty string"
    (is (= "" (v/group-id->path nil)))))
