(ns bb-depsolve.forge-test
  "Unit tests for multi-forge support and major-bump detection (v0.8.0)."
  (:require [clojure.test :refer [deftest is testing]]
            [bb-depsolve.version.api :as v]))

(deftest parse-forge-lib-test
  (testing "parses GitHub libs"
    (is (= {:forge :github :org "hive-agi" :repo "hive-mcp"}
           (v/parse-forge-lib 'io.github.hive-agi/hive-mcp))))
  (testing "parses GitLab libs"
    (is (= {:forge :gitlab :org "myorg" :repo "myrepo"}
           (v/parse-forge-lib 'io.gitlab.myorg/myrepo))))
  (testing "parses Codeberg libs"
    (is (= {:forge :codeberg :org "myorg" :repo "tool"}
           (v/parse-forge-lib 'io.codeberg.myorg/tool))))
  (testing "non-forge libs return nil"
    (is (nil? (v/parse-forge-lib 'cheshire/cheshire)))
    (is (nil? (v/parse-forge-lib 'reagent/reagent)))))

(deftest forge-clone-url-test
  (testing "github URL"
    (is (= "https://github.com/hive-agi/hive-mcp"
           (v/forge-clone-url :github "hive-agi" "hive-mcp"))))
  (testing "gitlab URL"
    (is (= "https://gitlab.com/foo/bar"
           (v/forge-clone-url :gitlab "foo" "bar"))))
  (testing "codeberg URL"
    (is (= "https://codeberg.org/foo/bar"
           (v/forge-clone-url :codeberg "foo" "bar"))))
  (testing "unknown forge returns nil"
    (is (nil? (v/forge-clone-url :sourcehut "foo" "bar")))))

(deftest forge-raw-url-test
  (testing "github raw URL uses raw.githubusercontent.com"
    (is (= "https://raw.githubusercontent.com/foo/bar/v1.0.0/deps.edn"
           (v/forge-raw-url :github "foo" "bar" "v1.0.0" "deps.edn"))))
  (testing "gitlab raw URL uses /-/raw/ shape"
    (is (= "https://gitlab.com/foo/bar/-/raw/v1.0.0/deps.edn"
           (v/forge-raw-url :gitlab "foo" "bar" "v1.0.0" "deps.edn"))))
  (testing "codeberg raw URL uses /raw/tag/ shape"
    (is (= "https://codeberg.org/foo/bar/raw/tag/v1.0.0/deps.edn"
           (v/forge-raw-url :codeberg "foo" "bar" "v1.0.0" "deps.edn"))))
  (testing "unknown forge returns nil"
    (is (nil? (v/forge-raw-url :unknown "foo" "bar" "v1.0.0" "deps.edn")))))

(deftest major-bump?-test
  (testing "post-1.0 major bump"
    (is (true? (v/major-bump? "v1.0.0" "v2.0.0")))
    (is (true? (v/major-bump? "v1.5.3" "v3.0.0"))))
  (testing "pre-1.0 bumps never count as major (0.x.x convention)"
    (is (false? (v/major-bump? "v0.4.0" "v0.5.0")))
    (is (false? (v/major-bump? "v0.4.0" "v1.0.0"))))
  (testing "minor/patch bumps are not major"
    (is (false? (v/major-bump? "v1.0.0" "v1.1.0")))
    (is (false? (v/major-bump? "v1.0.0" "v1.0.1"))))
  (testing "downgrade is not major bump"
    (is (false? (v/major-bump? "v2.0.0" "v1.0.0"))))
  (testing "equal version is not major bump"
    (is (false? (v/major-bump? "v1.0.0" "v1.0.0"))))
  (testing "unparseable returns false"
    (is (false? (v/major-bump? nil "v1.0.0")))
    (is (false? (v/major-bump? "v1.0.0" nil)))
    (is (false? (v/major-bump? "garbage" "v1.0.0")))))
