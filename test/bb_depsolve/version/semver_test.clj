(ns bb-depsolve.version.semver-test
  "Unit tests for semver arithmetic."
  (:require [clojure.test :refer [deftest is testing]]
            [bb-depsolve.version.api :as v]))

(deftest parse-semver-test
  (testing "standard semver with v prefix"
    (is (= [0 4 0] (v/parse-semver "v0.4.0")))
    (is (= [1 2 3] (v/parse-semver "v1.2.3"))))

  (testing "semver without v prefix"
    (is (= [1 0 0] (v/parse-semver "1.0.0")))
    (is (= [0 0 1] (v/parse-semver "0.0.1"))))

  (testing "semver with pre-release suffix"
    (is (= [1 0 0] (v/parse-semver "v1.0.0-alpha")))
    (is (= [2 1 0] (v/parse-semver "v2.1.0-rc1"))))

  (testing "non-semver returns nil"
    (is (nil? (v/parse-semver "latest")))
    (is (nil? (v/parse-semver "abc")))
    (is (nil? (v/parse-semver "")))))

(deftest version-newer?-test
  (testing "simple version comparisons"
    (is (true? (v/version-newer? "1.0.0" "1.0.1")))
    (is (true? (v/version-newer? "1.0.0" "1.1.0")))
    (is (true? (v/version-newer? "1.0.0" "2.0.0"))))

  (testing "equal versions"
    (is (false? (v/version-newer? "1.0.0" "1.0.0"))))

  (testing "older version is not newer"
    (is (false? (v/version-newer? "2.0.0" "1.0.0")))
    (is (false? (v/version-newer? "1.1.0" "1.0.0"))))

  (testing "different segment counts"
    (is (true? (v/version-newer? "1.0" "1.0.1")))
    (is (false? (v/version-newer? "1.0.1" "1.0"))))

  (testing "real-world versions"
    (is (true? (v/version-newer? "0.5.22" "0.5.30")))
    (is (true? (v/version-newer? "0.4.22" "0.4.23")))
    (is (false? (v/version-newer? "5.13.0" "5.12.0")))))

(deftest pre-release?-test
  (testing "pre-release markers"
    (is (true? (v/pre-release? "1.0.0-alpha")))
    (is (true? (v/pre-release? "1.0.0-beta1")))
    (is (true? (v/pre-release? "1.0.0-RC1")))
    (is (true? (v/pre-release? "1.0.0-SNAPSHOT")))
    (is (true? (v/pre-release? "2.0.0-preview"))))

  (testing "stable versions"
    (is (false? (v/pre-release? "1.0.0")))
    (is (false? (v/pre-release? "5.13.0")))
    (is (false? (v/pre-release? "0.5.30")))))

(deftest latest-tag-test
  (testing "finds the latest semver tag"
    (let [tags [{:tag "v0.1.0" :sha "aaa"}
                {:tag "v0.3.0" :sha "ccc"}
                {:tag "v0.2.0" :sha "bbb"}]]
      (is (= {:tag "v0.3.0" :sha "ccc"} (v/latest-tag tags)))))

  (testing "ignores non-semver tags"
    (let [tags [{:tag "v0.1.0" :sha "aaa"}
                {:tag "latest" :sha "xxx"}]]
      (is (= {:tag "v0.1.0" :sha "aaa"} (v/latest-tag tags)))))

  (testing "empty list returns nil"
    (is (nil? (v/latest-tag [])))))

(deftest bump-patch-test
  (testing "increments patch, preserves major and minor"
    (is (= [0 1 2] (v/bump-patch [0 1 1])))
    (is (= [0 0 1] (v/bump-patch [0 0 0])))
    (is (= [5 3 10] (v/bump-patch [5 3 9])))))

(deftest bump-minor-test
  (testing "increments minor, zeroes patch, preserves major"
    (is (= [0 2 0] (v/bump-minor [0 1 1])))
    (is (= [0 1 0] (v/bump-minor [0 0 5])))
    (is (= [3 4 0] (v/bump-minor [3 3 99])))))

(deftest bump-major-test
  (testing "increments major, zeroes minor and patch"
    (is (= [1 0 0] (v/bump-major [0 1 1])))
    (is (= [1 0 0] (v/bump-major [0 0 0])))
    (is (= [4 0 0] (v/bump-major [3 5 9])))))

(deftest semver->tag-test
  (testing "formats semver triple as v-prefixed tag"
    (is (= "v1.2.3" (v/semver->tag [1 2 3])))
    (is (= "v0.0.0" (v/semver->tag [0 0 0])))
    (is (= "v10.20.30" (v/semver->tag [10 20 30])))))

(deftest semver->version-test
  (testing "formats semver triple as version string"
    (is (= "1.2.3" (v/semver->version [1 2 3])))
    (is (= "0.0.0" (v/semver->version [0 0 0])))
    (is (= "10.20.30" (v/semver->version [10 20 30])))))

(deftest tag->mvn-version-test
  (testing "strips leading v"
    (is (= "0.3.6" (v/tag->mvn-version "v0.3.6")))
    (is (= "1.0.0-alpha" (v/tag->mvn-version "v1.0.0-alpha"))))

  (testing "no v prefix passes through"
    (is (= "0.3.6" (v/tag->mvn-version "0.3.6"))))

  (testing "nil returns nil"
    (is (nil? (v/tag->mvn-version nil)))))
