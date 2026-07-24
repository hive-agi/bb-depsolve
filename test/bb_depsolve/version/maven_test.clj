(ns bb-depsolve.version.maven-test
  "Unit tests for Maven metadata and POM handling."
  (:require [clojure.test :refer [deftest is testing]]
            [bb-depsolve.version :as v]
            [clojure.string :as str]))

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

(deftest maven-metadata-test
  (let [metadata "<metadata><versioning><latest>0.1.1</latest><versions>
     <version>0.1.1</version><version>0.1.3-rc1</version>
     <version>0.1.2</version><version>0.1.3</version>
   </versions></versioning></metadata>"]
    (testing "URL is canonical even when the configured base has trailing slashes"
      (is (= "https://registry.example/maven/io/github/hive-agi/hive-carto/maven-metadata.xml"
             (v/maven-metadata-url "https://registry.example/maven///"
                                   "io.github.hive-agi" "hive-carto"))))
    (testing "all published versions are parsed in registry order"
      (is (= ["0.1.1" "0.1.3-rc1" "0.1.2" "0.1.3"]
             (v/parse-maven-metadata-versions metadata))))
    (testing "max version wins over stale metadata <latest>"
      (is (= "0.1.3" (v/latest-published-version metadata {:allow-pre? false}))))
    (testing "the pre-release is available when asked for"
      (is (= "0.1.3-rc1" (v/latest-published-version metadata {:allow-pre? true}))))
    (testing "malformed or empty metadata is total"
      (is (= [] (v/parse-maven-metadata-versions "not xml")))
      (is (nil? (v/latest-published-version nil {:allow-pre? false}))))))
