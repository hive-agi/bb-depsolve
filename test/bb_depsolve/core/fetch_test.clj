(ns bb-depsolve.core.fetch-test
  "Tests for gated HTTP retrieval of POM and deps.edn artifacts. Every request
   goes through an injected `gated-http` transport whose GET and credential
   lookup are stubs, so no test reaches the network."
  (:require [bb-depsolve.core.fetch :as fetch]
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
  "Stand-in GET: serves BODY when PRED matches the url, 404 otherwise.
   Every [url opts] pair is recorded into the CALLS atom."
  [calls pred body]
  (fn [url opts]
    (swap! calls conj [url opts])
    (if (pred url)
      {:status 200 :body body}
      {:status 404 :body ""})))

(defn- transport
  "A gated-http transport over `stub-get`, with no credentials unless
   HEADERS-FN is given."
  ([calls pred body]
   (transport calls pred body (constantly nil)))
  ([calls pred body headers-fn]
   (fetch/gated-http {:get-fn (stub-get calls pred body)
                      :headers-fn headers-fn})))

(defn- urls [calls] (mapv first @calls))

;; =============================================================================
;; Unit:gated-http
;; =============================================================================

(deftest gated-http-sends-headers-only-when-the-target-has-credentials-test
  (let [calls (atom [])
        t (transport calls (constantly true) "body"
                     (fn [target] (when (= :clojars target) {"Authorization" "Basic zzz"})))]
    (is (= 200 (:status (fetch/http-get t "https://x/clojars" :clojars))))
    (is (= 200 (:status (fetch/http-get t "https://x/other" :none))))
    (is (= [{:throw false :headers {"Authorization" "Basic zzz"}}
            {:throw false}]
           (mapv second @calls))
        "a target with no credentials sends no :headers key at all")))

;; =============================================================================
;; Unit:fetch-pom-deps
;; =============================================================================

(deftest fetch-pom-deps-reads-a-published-pom-test
  (let [calls (atom [])
        t (transport calls #(str/includes? % "clojars") pom)]
    (is (= {:ok [{:lib 'org.clojure/clojure :version "1.12.1"}]}
           (fetch/fetch-pom-deps t "org.clojure" "clojure" "1.12.1"))
        "a 200 response is parsed, not reported as missing")
    (is (= 1 (count (urls calls)))
        "Maven Central is not queried once Clojars answers")))

(deftest fetch-pom-deps-drops-unresolved-property-coords-test
  (let [calls (atom [])
        t (transport calls (constantly true) pom)]
    (is (= [{:lib 'org.clojure/clojure :version "1.12.1"}]
           (:ok (fetch/fetch-pom-deps t "org.clojure" "clojure" "1.12.1")))
        "acme/lib still carries a ${...} placeholder")))

(deftest fetch-pom-deps-falls-back-to-maven-central-test
  (let [calls (atom [])
        t (transport calls #(str/includes? % "repo1.maven.org") pom)]
    (is (= [{:lib 'org.clojure/clojure :version "1.12.1"}]
           (:ok (fetch/fetch-pom-deps t "org.clojure" "clojure" "1.12.1"))))
    (is (= 2 (count (urls calls)))
        "Clojars is tried first, Maven Central second")))

(deftest fetch-pom-deps-errs-when-no-registry-has-the-artifact-test
  (let [calls (atom [])
        t (transport calls (constantly false) pom)]
    (is (= :io/fetch-pom
           (:error (fetch/fetch-pom-deps t "org.clojure" "clojure" "1.12.1"))))))

(deftest fetch-pom-deps-sends-registry-credentials-test
  (let [calls (atom [])
        t (transport calls #(str/includes? % "clojars") pom
                     (fn [target] (when (= :clojars target)
                                    {"Authorization" "Basic zzz"})))]
    (fetch/fetch-pom-deps t "org.clojure" "clojure" "1.12.1")
    (is (= {"Authorization" "Basic zzz"} (:headers (second (first @calls))))
        "the clojars url picks the :clojars credential target")))

(deftest fetch-pom-deps-errs-when-the-transport-throws-test
  (let [t (reify fetch/IHttpTransport
            (http-get [_ _ _] (throw (ex-info "connection reset" {}))))]
    (is (= :io/fetch-pom
           (:error (fetch/fetch-pom-deps t "org.clojure" "clojure" "1.12.1")))
        "a failed request is an error Result, not an exception")))

(def ^:private child-pom
  (str "<project><parent><groupId>acme</groupId><artifactId>acme-parent</artifactId>"
       "<version>3.0.0</version></parent><artifactId>child</artifactId>"
       "<dependencies>"
       "<dependency><groupId>${project.groupId}</groupId><artifactId>sibling</artifactId>"
       "<version>${project.version}</version></dependency>"
       "<dependency><groupId>org.slf4j</groupId><artifactId>slf4j-api</artifactId>"
       "<version>${slf4j.version}</version></dependency>"
       "</dependencies></project>"))

(def ^:private parent-pom
  (str "<project><groupId>acme</groupId><artifactId>acme-parent</artifactId>"
       "<version>3.0.0</version><properties><slf4j.version>2.0.13</slf4j.version>"
       "</properties></project>"))

(defn- serve
  "Stand-in GET serving the first BODY whose url fragment the url contains."
  [calls routes]
  (fn [url opts]
    (swap! calls conj [url opts])
    (if-let [body (some (fn [[frag body]] (when (str/includes? url frag) body)) routes)]
      {:status 200 :body body}
      {:status 404 :body ""})))

(deftest fetch-pom-deps-resolves-properties-from-the-pom-and-its-parent-test
  (let [calls (atom [])
        t (fetch/gated-http {:get-fn (serve calls [["acme-parent-3.0.0.pom" parent-pom]
                                                     ["child-3.0.0.pom" child-pom]])
                             :headers-fn (constantly nil)})]
    (is (= {:ok [{:lib 'acme/sibling :version "3.0.0"}
                 {:lib 'org.slf4j/slf4j-api :version "2.0.13"}]}
           (fetch/fetch-pom-deps t "acme" "child" "3.0.0"))
        "${project.*} resolves from the POM itself, ${slf4j.version} from its parent")
    (is (some #(str/includes? % "acme-parent-3.0.0.pom") (urls calls))
        "the parent POM is fetched only because something stayed unresolved")))

(deftest fetch-pom-deps-survives-an-unreachable-parent-test
  (let [calls (atom [])
        t (fetch/gated-http {:get-fn (serve calls [["child-3.0.0.pom" child-pom]])
                             :headers-fn (constantly nil)})]
    (is (= {:ok [{:lib 'acme/sibling :version "3.0.0"}]}
           (fetch/fetch-pom-deps t "acme" "child" "3.0.0"))
        "a missing parent leaves slf4j unresolved (dropped and warned), not an error")))

(deftest fetch-pom-deps-does-not-fetch-a-parent-when-nothing-is-unresolved-test
  (let [calls (atom [])
        resolved (str/replace child-pom "${slf4j.version}" "2.0.0")
        t (fetch/gated-http {:get-fn (serve calls [["child-3.0.0.pom" resolved]
                                                     ["acme-parent" parent-pom]])
                             :headers-fn (constantly nil)})]
    (fetch/fetch-pom-deps t "acme" "child" "3.0.0")
    (is (not-any? #(str/includes? % "acme-parent") (urls calls)))))

;; =============================================================================
;; Unit:fetch-git-deps-edn / fetch-git-dep-coords
;; =============================================================================

(deftest fetch-git-deps-edn-reads-raw-content-test
  (let [calls (atom [])
        t (transport calls (constantly true) deps-edn)]
    (is (= {:ok deps-edn} (fetch/fetch-git-deps-edn t :github "hive-agi" "hive-dsl" "v0.5.8")))
    (is (= ["https://raw.githubusercontent.com/hive-agi/hive-dsl/v0.5.8/deps.edn"]
           (urls calls)))))

(deftest fetch-git-deps-edn-sends-the-forge-as-credential-target-test
  (let [targets (atom [])
        t (reify fetch/IHttpTransport
            (http-get [_ _ target] (swap! targets conj target) {:status 200 :body deps-edn}))]
    (fetch/fetch-git-deps-edn t :github "hive-agi" "hive-dsl" "v0.5.8")
    (is (= [:github] @targets))))

(deftest fetch-git-deps-edn-errs-test
  (testing "an unsupported forge never reaches the network"
    (let [calls (atom [])
          t (transport calls (constantly true) deps-edn)]
      (is (= :io/fetch-git-deps
             (:error (fetch/fetch-git-deps-edn t :bitbucket "org" "repo" "v1"))))
      (is (= [] (urls calls)))))
  (testing "a missing file is an error, not an empty body"
    (let [calls (atom [])
          t (transport calls (constantly false) deps-edn)]
      (is (= :io/fetch-git-deps
             (:error (fetch/fetch-git-deps-edn t :github "org" "repo" "v1")))))))

(deftest fetch-git-dep-coords-parses-the-fetched-deps-edn-test
  (let [calls (atom [])
        t (transport calls (constantly true) deps-edn)]
    (is (= {:ok [{:lib 'io.github.hive-agi/hive-dsl :version "0.5.8" :type :mvn}]}
           (fetch/fetch-git-dep-coords t "hive-agi" "hive-dsl" "v0.5.8")))
    (is (str/includes? (first (urls calls)) "raw.githubusercontent.com")
        "a git dep's deps.edn is read from GitHub")))
