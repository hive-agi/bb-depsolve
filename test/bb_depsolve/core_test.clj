(ns bb-depsolve.core-test
  "Unit tests for bb-depsolve pure calculations (version.clj layer)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [bb-depsolve.version :as v]))

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

(deftest sha-matches?-test
  (testing "prefix match"
    (is (true? (v/sha-matches? "abc1234" "abc1234567890")))
    (is (true? (v/sha-matches? "abc1234567890" "abc1234"))))
  (testing "mismatch"
    (is (false? (v/sha-matches? "abc" "def"))))
  (testing "nil safety"
    (is (nil? (v/sha-matches? nil "abc")))))

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

;; =============================================================================
;; Transitive dependency resolution (v0.5.0)
;; =============================================================================

(deftest maven-property?-test
  (testing "detects Maven property placeholders"
    (is (true? (v/maven-property? "${clojure.version}")))
    (is (true? (v/maven-property? "${project.version}")))
    (is (true? (v/maven-property? "${jackson.version}")))
    (is (true? (v/maven-property? "${foo}"))))

  (testing "rejects normal version strings"
    (is (false? (v/maven-property? "1.0.0")))
    (is (false? (v/maven-property? "2.17.0")))
    (is (false? (v/maven-property? "1.0.0-SNAPSHOT")))
    (is (false? (v/maven-property? "v0.4.0"))))

  (testing "rejects nil and empty"
    (is (false? (v/maven-property? nil)))
    (is (false? (v/maven-property? ""))))

  (testing "rejects partial placeholders"
    (is (false? (v/maven-property? "${}")))
    (is (false? (v/maven-property? "$foo")))
    (is (false? (v/maven-property? "foo${bar}")))
    (is (false? (v/maven-property? "${bar}baz")))))

(deftest unresolved-property?-test
  (testing "string arity — detects ${...} placeholders anywhere"
    (is (true?  (v/unresolved-property? "${foo}")))
    (is (true?  (v/unresolved-property? "${clojure.version}")))
    (is (true?  (v/unresolved-property? "foo-${bar}-baz")))
    (is (false? (v/unresolved-property? "1.2.3")))
    (is (false? (v/unresolved-property? "v0.4.0")))
    (is (false? (v/unresolved-property? "1.0.0-SNAPSHOT")))
    (is (false? (v/unresolved-property? ""))))

  (testing "nil and non-string/non-map inputs return false (total)"
    (is (false? (v/unresolved-property? nil)))
    (is (false? (v/unresolved-property? 42)))
    (is (false? (v/unresolved-property? [])))
    (is (false? (v/unresolved-property? :kw))))

  (testing "coord-map arity — checks key fields"
    (is (true?  (v/unresolved-property? {:lib 'org.clojure/clojure
                                         :version "${clojure.version}"})))
    (is (true?  (v/unresolved-property? {:group "${parent.groupId}"
                                         :artifact "foo" :version "1.0"})))
    (is (true?  (v/unresolved-property? {:artifact "${name}" :version "1.0"})))
    (is (false? (v/unresolved-property? {:lib 'cheshire/cheshire :version "5.13.0"})))
    (is (false? (v/unresolved-property? {:group "org.clojure" :artifact "clojure"
                                         :version "1.12.0"})))))

(deftest filter-resolved-coords-test
  (testing "drops coords with unresolved ${...} and keeps resolved"
    (let [coords [{:lib 'a/a :version "1.0.0"}
                  {:lib 'b/b :version "${b.version}"}
                  {:lib 'c/c :version "2.0.0"}
                  {:lib 'd/d :version "${d.version}"}]
          filtered (v/filter-resolved-coords coords)]
      (is (= 2 (count filtered)))
      (is (= #{'a/a 'c/c} (set (map :lib filtered))))))

  (testing "preserves order of resolved coords"
    (let [coords [{:lib 'a/a :version "1.0"}
                  {:lib 'b/b :version "${x}"}
                  {:lib 'c/c :version "2.0"}]]
      (is (= ['a/a 'c/c]
             (mapv :lib (v/filter-resolved-coords coords))))))

  (testing "2-arity warn-fn is invoked once per dropped coord"
    (let [warnings (atom [])
          warn-fn  #(swap! warnings conj %)
          coords   [{:lib 'ok/ok :version "1.0"}
                    {:lib 'bad/one :version "${v1}"}
                    {:lib 'bad/two :version "${v2}"}]
          filtered (v/filter-resolved-coords coords warn-fn)]
      (is (= 1 (count filtered)))
      (is (= 2 (count @warnings)))
      (is (= #{'bad/one 'bad/two} (set (map :lib @warnings))))))

  (testing "empty / nil inputs are total"
    (is (= [] (v/filter-resolved-coords [])))
    (is (= [] (v/filter-resolved-coords nil))))

  (testing "idempotent — filtering twice equals filtering once"
    (let [coords [{:lib 'a/a :version "1.0"}
                  {:lib 'b/b :version "${x}"}
                  {:lib 'c/c :version "2.0"}]
          once  (v/filter-resolved-coords coords)
          twice (v/filter-resolved-coords once)]
      (is (= once twice)))))

(deftest parse-pom-deps-raw-keeps-unresolved-test
  (testing "raw variant returns all compile-scope deps incl. ${...} placeholders"
    (let [pom "<?xml version=\"1.0\"?>
<project>
  <dependencies>
    <dependency>
      <groupId>com.fasterxml.jackson.core</groupId>
      <artifactId>jackson-core</artifactId>
      <version>2.17.0</version>
    </dependency>
    <dependency>
      <groupId>org.clojure</groupId>
      <artifactId>clojure</artifactId>
      <version>${clojure.version}</version>
    </dependency>
  </dependencies>
</project>"
          raw (v/parse-pom-deps-raw pom)
          filtered (v/filter-resolved-coords raw)]
      (is (= 2 (count raw)) "raw keeps unresolved ${...} coord")
      (is (= 1 (count filtered)) "filter drops the ${...} coord")
      (is (= 'com.fasterxml.jackson.core/jackson-core
             (:lib (first filtered)))))))

(deftest parse-pom-deps-filters-maven-properties-test
  (testing "filters out deps with unresolved Maven property versions"
    (let [pom "<?xml version=\"1.0\"?>
<project>
  <dependencies>
    <dependency>
      <groupId>com.fasterxml.jackson.core</groupId>
      <artifactId>jackson-core</artifactId>
      <version>2.17.0</version>
    </dependency>
    <dependency>
      <groupId>org.clojure</groupId>
      <artifactId>clojure</artifactId>
      <version>${clojure.version}</version>
    </dependency>
    <dependency>
      <groupId>some.group</groupId>
      <artifactId>some-lib</artifactId>
      <version>${project.version}</version>
    </dependency>
  </dependencies>
</project>"
          deps (v/parse-pom-deps pom)]
      (is (= 1 (count deps)))
      (is (= 'com.fasterxml.jackson.core/jackson-core (:lib (first deps))))
      (is (= "2.17.0" (:version (first deps))))))

  (testing "keeps all deps when none use property placeholders"
    (let [pom "<?xml version=\"1.0\"?>
<project>
  <dependencies>
    <dependency>
      <groupId>org.clojure</groupId>
      <artifactId>clojure</artifactId>
      <version>1.11.1</version>
    </dependency>
    <dependency>
      <groupId>cheshire</groupId>
      <artifactId>cheshire</artifactId>
      <version>5.13.0</version>
    </dependency>
  </dependencies>
</project>"
          deps (v/parse-pom-deps pom)]
      (is (= 2 (count deps))))))

(deftest group-id->path-test
  (testing "converts dots to slashes"
    (is (= "com/fasterxml/jackson/core" (v/group-id->path "com.fasterxml.jackson.core"))))
  (testing "simple group"
    (is (= "cheshire" (v/group-id->path "cheshire"))))
  (testing "nil returns empty string"
    (is (= "" (v/group-id->path nil)))))

(deftest pom-urls-test
  (testing "generates clojars and maven central URLs"
    (let [[clojars maven] (v/pom-urls "cheshire" "cheshire" "6.1.0")]
      (is (str/includes? clojars "repo.clojars.org"))
      (is (str/includes? clojars "cheshire/cheshire/6.1.0/cheshire-6.1.0.pom"))
      (is (str/includes? maven "repo1.maven.org"))
      (is (str/includes? maven "cheshire/cheshire/6.1.0/cheshire-6.1.0.pom"))))
  (testing "nested group path"
    (let [[clojars _] (v/pom-urls "com.fasterxml.jackson.core" "jackson-core" "2.20.0")]
      (is (str/includes? clojars "com/fasterxml/jackson/core/jackson-core/2.20.0")))))

(deftest parse-pom-deps-test
  (testing "parses compile-scope deps from POM XML"
    (let [pom "<?xml version=\"1.0\"?>
<project>
  <dependencies>
    <dependency>
      <groupId>com.fasterxml.jackson.core</groupId>
      <artifactId>jackson-core</artifactId>
      <version>2.17.0</version>
    </dependency>
    <dependency>
      <groupId>junit</groupId>
      <artifactId>junit</artifactId>
      <version>4.13</version>
      <scope>test</scope>
    </dependency>
  </dependencies>
</project>"
          deps (v/parse-pom-deps pom)]
      (is (= 1 (count deps)))
      (is (= 'com.fasterxml.jackson.core/jackson-core (:lib (first deps))))
      (is (= "2.17.0" (:version (first deps))))))

  (testing "skips optional deps"
    (let [pom "<?xml version=\"1.0\"?>
<project>
  <dependencies>
    <dependency>
      <groupId>org.clojure</groupId>
      <artifactId>clojure</artifactId>
      <version>1.12.0</version>
    </dependency>
    <dependency>
      <groupId>opt</groupId>
      <artifactId>opt-lib</artifactId>
      <version>1.0</version>
      <optional>true</optional>
    </dependency>
  </dependencies>
</project>"
          deps (v/parse-pom-deps pom)]
      (is (= 1 (count deps)))
      (is (= 'org.clojure/clojure (:lib (first deps))))))

  (testing "returns empty for nil"
    (is (= [] (v/parse-pom-deps nil))))

  (testing "returns empty for garbage input"
    (is (= [] (v/parse-pom-deps "not xml at all")))))

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

;; =============================================================================
;; Integration: real fixture POM with ${revision} + CI-friendly property refs
;; =============================================================================

(deftest unresolved-property-integration-test
  (testing "parsing a real fixture POM with ${revision}/${jackson.version} drops"
    (let [pom-xml  (slurp "test/fixtures/unresolved-revision.pom.xml")
          raw      (v/parse-pom-deps-raw pom-xml)
          warnings (atom [])
          filtered (v/filter-resolved-coords raw #(swap! warnings conj %))
          libs     (set (map :lib filtered))
          warned   (set (map :lib @warnings))]
      ;; Raw parse sees all 4 compile deps (no silent drops).
      (is (= 4 (count raw)))
      ;; Filter keeps only the two resolved coords.
      (is (= 2 (count filtered)))
      (is (contains? libs 'org.clojure/clojure))
      (is (contains? libs 'cheshire/cheshire))
      ;; Filter drops both unresolved coords — and logs each.
      (is (= 2 (count @warnings)))
      (is (contains? warned 'example.unresolved/sibling-module))
      (is (contains? warned 'com.fasterxml.jackson.core/jackson-databind))
      ;; Dropped coords truly contained ${...} placeholders.
      (is (every? v/unresolved-property? @warnings)))))
