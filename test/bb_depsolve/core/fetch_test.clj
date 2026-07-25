(ns bb-depsolve.core.fetch-test
  "Tests for gated HTTP retrieval of POM and deps.edn artifacts."
  (:require [babashka.http-client :as http]
            [bb-depsolve.core.auth :as auth]
            [bb-depsolve.core.fetch :as fetch]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private pom
  (str "<project><dependencies>"
       "<dependency><groupId>org.clojure</groupId><artifactId>clojure</artifactId>"
       "<version>1.12.1</version></dependency>"
       "<dependency><groupId>acme</groupId><artifactId>lib</artifactId>"
       "<version>${acme.version}</version></dependency>"
       "</dependencies></project>"))

(def ^:private deps-edn
  "{:deps {io.github.hive-agi/hive-dsl {:mvn/version \"0.5.8\"}}}")

(defn- stub-get
  "Stand-in for http/get: serves BODY when PRED matches the url, 404 otherwise.
   Every [url opts] pair is recorded into the CALLS atom."
  [calls pred body]
  (fn [url & [opts]]
    (swap! calls conj [url opts])
    (if (pred url)
      {:status 200 :body body}
      {:status 404 :body ""})))

(defn- urls [calls] (mapv first @calls))

;; =============================================================================
;; Unit — fetch-pom-deps
;; =============================================================================

(deftest fetch-pom-deps-reads-a-published-pom-test
  (let [calls (atom [])]
    (with-redefs [http/get (stub-get calls #(str/includes? % "clojars") pom)]
      (is (= {:ok [{:lib 'org.clojure/clojure :version "1.12.1"}]}
             (fetch/fetch-pom-deps "org.clojure" "clojure" "1.12.1"))
          "a 200 response is parsed, not reported as missing")
      (is (= 1 (count (urls calls)))
          "Maven Central is not queried once Clojars answers"))))

(deftest fetch-pom-deps-drops-unresolved-property-coords-test
  (let [calls (atom [])]
    (with-redefs [http/get (stub-get calls (constantly true) pom)]
      (is (= [{:lib 'org.clojure/clojure :version "1.12.1"}]
             (:ok (fetch/fetch-pom-deps "org.clojure" "clojure" "1.12.1")))
          "acme/lib still carries a ${...} placeholder"))))

(deftest fetch-pom-deps-falls-back-to-maven-central-test
  (let [calls (atom [])]
    (with-redefs [http/get (stub-get calls #(str/includes? % "repo1.maven.org") pom)]
      (is (= [{:lib 'org.clojure/clojure :version "1.12.1"}]
             (:ok (fetch/fetch-pom-deps "org.clojure" "clojure" "1.12.1"))))
      (is (= 2 (count (urls calls)))
          "Clojars is tried first, Maven Central second"))))

(deftest fetch-pom-deps-errs-when-no-registry-has-the-artifact-test
  (let [calls (atom [])]
    (with-redefs [http/get (stub-get calls (constantly false) pom)]
      (is (= :io/fetch-pom
             (:error (fetch/fetch-pom-deps "org.clojure" "clojure" "1.12.1")))))))

(deftest fetch-pom-deps-sends-registry-credentials-test
  (let [calls (atom [])]
    (with-redefs [http/get (stub-get calls #(str/includes? % "clojars") pom)
                  auth/auth-headers (fn [target] (when (= :clojars target)
                                                   {"Authorization" "Basic zzz"}))]
      (fetch/fetch-pom-deps "org.clojure" "clojure" "1.12.1")
      (is (= {"Authorization" "Basic zzz"} (:headers (second (first @calls))))
          "the clojars url picks the :clojars credential target"))))

;; =============================================================================
;; Unit — fetch-git-deps-edn / fetch-git-dep-coords
;; =============================================================================

(deftest fetch-git-deps-edn-reads-raw-content-test
  (let [calls (atom [])]
    (with-redefs [http/get (stub-get calls (constantly true) deps-edn)]
      (is (= {:ok deps-edn} (fetch/fetch-git-deps-edn :github "hive-agi" "hive-dsl" "v0.5.8")))
      (is (= ["https://raw.githubusercontent.com/hive-agi/hive-dsl/v0.5.8/deps.edn"]
             (urls calls))))))

(deftest fetch-git-deps-edn-defaults-to-github-test
  (let [calls (atom [])]
    (with-redefs [http/get (stub-get calls (constantly true) deps-edn)]
      (is (= {:ok deps-edn} (fetch/fetch-git-deps-edn "hive-agi" "hive-dsl" "v0.5.8")))
      (is (str/includes? (first (urls calls)) "raw.githubusercontent.com")))))

(deftest fetch-git-deps-edn-errs-test
  (testing "an unsupported forge never reaches the network"
    (let [calls (atom [])]
      (with-redefs [http/get (stub-get calls (constantly true) deps-edn)]
        (is (= :io/fetch-git-deps
               (:error (fetch/fetch-git-deps-edn :bitbucket "org" "repo" "v1"))))
        (is (= [] (urls calls))))))
  (testing "a missing file is an error, not an empty body"
    (let [calls (atom [])]
      (with-redefs [http/get (stub-get calls (constantly false) deps-edn)]
        (is (= :io/fetch-git-deps
               (:error (fetch/fetch-git-deps-edn :github "org" "repo" "v1"))))))))

(deftest fetch-git-dep-coords-parses-the-fetched-deps-edn-test
  (let [calls (atom [])]
    (with-redefs [http/get (stub-get calls (constantly true) deps-edn)]
      (is (= {:ok [{:lib 'io.github.hive-agi/hive-dsl :version "0.5.8" :type :mvn}]}
             (fetch/fetch-git-dep-coords "hive-agi" "hive-dsl" "v0.5.8"))))))
