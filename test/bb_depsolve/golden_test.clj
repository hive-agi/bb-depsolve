(ns bb-depsolve.golden-test
  "Golden/characterization tests for bb-depsolve pure functions.

   Snapshots output shapes of key functions to detect unintended
   behavioral changes during refactoring. Uses hive-test.golden.

   First run creates golden EDN files. Subsequent runs compare.
   UPDATE_GOLDEN=true regenerates all snapshots."
  (:require [bb-depsolve.version.api :as v]
            [clojure.string :as str]
            [hive-test.golden :as golden]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- strip-ansi
  "Remove ANSI escape codes from a string for deterministic golden snapshots."
  [s]
  (str/replace s #"\033\[[0-9;]*m" ""))

;; =============================================================================
;; Sample data — deterministic fixtures used across golden tests
;; =============================================================================

(def ^:private sample-pom
  "A representative POM XML with compile, test, optional, and property deps."
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<project>
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.example</groupId>
  <artifactId>sample-lib</artifactId>
  <version>1.0.0</version>
  <dependencies>
    <dependency>
      <groupId>org.clojure</groupId>
      <artifactId>clojure</artifactId>
      <version>1.12.0</version>
    </dependency>
    <dependency>
      <groupId>com.fasterxml.jackson.core</groupId>
      <artifactId>jackson-databind</artifactId>
      <version>2.17.0</version>
    </dependency>
    <dependency>
      <groupId>cheshire</groupId>
      <artifactId>cheshire</artifactId>
      <version>5.13.0</version>
    </dependency>
    <dependency>
      <groupId>junit</groupId>
      <artifactId>junit</artifactId>
      <version>4.13.2</version>
      <scope>test</scope>
    </dependency>
    <dependency>
      <groupId>org.slf4j</groupId>
      <artifactId>slf4j-api</artifactId>
      <version>2.0.9</version>
      <scope>provided</scope>
    </dependency>
    <dependency>
      <groupId>opt.group</groupId>
      <artifactId>opt-lib</artifactId>
      <version>1.0.0</version>
      <optional>true</optional>
    </dependency>
    <dependency>
      <groupId>org.clojure</groupId>
      <artifactId>tools.logging</artifactId>
      <version>${clojure.version}</version>
    </dependency>
  </dependencies>
</project>")

(def ^:private sample-deps-edn
  "A representative deps.edn string with mvn, git, and local deps."
  "{:deps {cheshire/cheshire          {:mvn/version \"6.1.0\"}
         org.clojure/clojure         {:mvn/version \"1.12.0\"}
         io.github.hive-agi/hive-dsl {:git/tag \"v0.3.7\" :git/sha \"d5deced\"}
         io.github.hive-agi/hive-events {:local/root \"../hive-events\"}}}")

(def ^:private sample-conflict-tree
  "A dependency tree with known version conflicts for c/c and e/e."
  [{:lib 'a/a :version "1.0.0" :cycle? false
    :children [{:lib 'c/c :version "1.0.0" :cycle? false :children []}
               {:lib 'd/d :version "3.0.0" :cycle? false
                :children [{:lib 'e/e :version "1.0.0" :cycle? false :children []}]}]}
   {:lib 'b/b :version "2.0.0" :cycle? false
    :children [{:lib 'c/c :version "2.0.0" :cycle? false :children []}
               {:lib 'e/e :version "2.0.0" :cycle? false :children []}]}])

(def ^:private sample-format-tree
  "A dependency tree for format-dep-tree testing (with cycles and nesting)."
  [{:lib 'org.clojure/clojure :version "1.12.0" :cycle? false
    :children [{:lib 'org.clojure/spec.alpha :version "0.3.218" :cycle? false
                :children []}
               {:lib 'org.clojure/core.specs.alpha :version "0.2.62" :cycle? false
                :children []}]}
   {:lib 'cheshire/cheshire :version "6.1.0" :cycle? false
    :children [{:lib 'com.fasterxml.jackson.core/jackson-core :version "2.17.0"
                :cycle? false :children []}
               {:lib 'tigris/tigris :version "0.1.2" :cycle? false :children []}]}
   {:lib 'cyclic/dep :version "0.1.0" :cycle? true :children []}])

;; =============================================================================
;; Golden: parse-pom-deps output shape
;; =============================================================================

(golden/deftest-golden parse-pom-deps-golden
  "test/golden/bb-depsolve/parse-pom-deps.edn"
  (v/parse-pom-deps sample-pom))

;; =============================================================================
;; Golden: deps-edn->dep-coords output shape
;; =============================================================================

(golden/deftest-golden deps-edn->dep-coords-golden
  "test/golden/bb-depsolve/deps-edn-dep-coords.edn"
  (v/deps-edn->dep-coords sample-deps-edn))

;; =============================================================================
;; Golden: find-conflicts output shape
;; =============================================================================

(golden/deftest-golden find-conflicts-golden
  "test/golden/bb-depsolve/find-conflicts.edn"
  (v/find-conflicts sample-conflict-tree))

;; =============================================================================
;; Golden: format-dep-tree output shape (ANSI stripped for determinism)
;; =============================================================================

(golden/deftest-golden-fn format-dep-tree-golden
  "test/golden/bb-depsolve/format-dep-tree.edn"
  (fn []
    (let [conflicts (v/find-conflicts sample-format-tree)
          lines     (v/format-dep-tree sample-format-tree conflicts)]
      (mapv strip-ansi lines))))

;; =============================================================================
;; Golden: pom-urls output shape
;; =============================================================================

(golden/deftest-golden pom-urls-golden
  "test/golden/bb-depsolve/pom-urls.edn"
  {:simple-group (v/pom-urls "cheshire" "cheshire" "6.1.0")
   :nested-group (v/pom-urls "com.fasterxml.jackson.core" "jackson-databind" "2.17.0")
   :clojure      (v/pom-urls "org.clojure" "clojure" "1.12.0")})
