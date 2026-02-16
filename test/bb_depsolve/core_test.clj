(ns bb-depsolve.core-test
  "Unit tests for bb-depsolve pure calculations (version.clj layer)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [bb-depsolve.version :as v]))

;; =============================================================================
;; parse-semver
;; =============================================================================

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

;; =============================================================================
;; version-newer?
;; =============================================================================

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

;; =============================================================================
;; pre-release? / stable?
;; =============================================================================

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

;; =============================================================================
;; find-git-deps / find-mvn-deps
;; =============================================================================

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

;; =============================================================================
;; update-git-dep / update-mvn-dep
;; =============================================================================

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

;; =============================================================================
;; latest-tag
;; =============================================================================

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

;; =============================================================================
;; parse-github-lib / lib-matches-org? / lib-artifact-id
;; =============================================================================

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

;; =============================================================================
;; sha-matches? / pick-sha
;; =============================================================================

(deftest sha-matches?-test
  (testing "prefix match"
    (is (true? (v/sha-matches? "abc1234" "abc1234567890")))
    (is (true? (v/sha-matches? "abc1234567890" "abc1234"))))
  (testing "mismatch"
    (is (false? (v/sha-matches? "abc" "def"))))
  (testing "nil safety"
    (is (nil? (v/sha-matches? nil "abc")))))

;; =============================================================================
;; find-local-deps
;; =============================================================================

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

;; =============================================================================
;; replace-local-with-git / replace-local-with-mvn
;; =============================================================================

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
