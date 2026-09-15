(ns bb-depsolve.core.auth-test
  "Tests for forge credentials and the private-registry endpoint. The registry
   comes from a throwaway workspace plus an explicit settings.xml, so no test
   reads ~/.m2. Assertions that depend on the environment being free of a
   credential run only when that variable is unset."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.auth :as auth]
            [clojure.test :refer [deftest is testing use-fixtures]]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private gitea-url "https://forge.example/api/packages/acme/maven")

(defn- restore-discovered
  "`use-workspace!` installs into a namespace-level atom; put back whatever the
   process held before the test."
  [f]
  (let [discovered @#'auth/discovered
        prior @discovered]
    (try (f) (finally (reset! discovered prior)))))

(use-fixtures :each restore-discovered)

(defn- workspace
  "A root deps.edn declaring REPOS, and a settings.xml granting u/p to
   `acme-forge`. => opts for `use-workspace!`."
  [repos]
  (let [root (str (fs/create-temp-dir {:prefix "bb-depsolve-auth"}))
        settings (str (fs/path root "settings.xml"))]
    (spit (str (fs/path root "deps.edn")) (pr-str {:mvn/repos repos :deps {}}))
    (spit settings (str "<settings><servers><server><id>acme-forge</id>"
                        "<username>u</username><password>p</password>"
                        "</server></servers></settings>"))
    {:root root :depth 0 :settings settings}))

(defn- env-unset? [& names] (every? #(nil? (System/getenv %)) names))

(defn- basic-of [user pass]
  (str "Basic " (.encodeToString (java.util.Base64/getEncoder)
                                 (.getBytes (str user ":" pass)))))

;; =============================================================================
;; basic
;; =============================================================================

(deftest basic-header-test
  (is (= {"Authorization" "Basic dTpw"} (#'auth/basic "u" "p")))
  (testing "half a credential is no credential"
    (is (nil? (#'auth/basic "u" nil)))
    (is (nil? (#'auth/basic nil "p")))
    (is (nil? (#'auth/basic nil nil)))))

;; =============================================================================
;; use-workspace! / private-registry / gitea-registry-url
;; =============================================================================

(deftest use-workspace!-installs-the-discovered-registry-test
  (when (env-unset? "MAVEN_URL")
    (let [reg (auth/use-workspace! (workspace {"acme-forge" {:url gitea-url}}))]
      (is (= {:id "acme-forge" :url gitea-url :username "u" :password "p"} reg))
      (is (= reg (auth/private-registry))
          "the installed registry is what later calls read")
      (is (= gitea-url (auth/gitea-registry-url)))
      (is (= {"Authorization" (basic-of "u" "p")} (auth/auth-headers :gitea))))))

(deftest use-workspace!-without-credentials-disables-the-registry-test
  (when (env-unset? "MAVEN_URL")
    (auth/use-workspace! (workspace {"acme-forge" {:url gitea-url}}))
    (testing "a later workspace that declares no usable registry replaces the old one"
      (is (nil? (auth/use-workspace! (workspace {"other-forge" {:url "https://other.example/m2"}}))))
      (is (nil? (auth/private-registry)))
      (is (nil? (auth/gitea-registry-url)))
      (is (nil? (auth/auth-headers :gitea))))
    (testing "public repositories never count as a private registry"
      (is (nil? (auth/use-workspace!
                 (workspace {"central" {:url "https://repo1.maven.org/maven2/"}})))))))

;; =============================================================================
;; auth-headers
;; =============================================================================

(deftest auth-headers-unknown-target-is-empty-test
  (is (= {} (auth/auth-headers :bitbucket)))
  (is (= {} (auth/auth-headers nil))))

(deftest auth-headers-without-env-credentials-test
  (testing "each forge target yields nothing when its variable is absent"
    (doseq [[target vars] {:github   ["GITHUB_TOKEN"]
                           :gitlab   ["GITLAB_TOKEN"]
                           :codeberg ["CODEBERG_TOKEN"]
                           :clojars  ["CLOJARS_USERNAME" "CLOJARS_PASSWORD"]
                           :maven    ["MAVEN_AUTH"]}
            :when (apply env-unset? vars)]
      (is (nil? (auth/auth-headers target)) (str target)))))

(deftest github-url-template-test
  (is (= "https://github.com/hive-agi/bb-depsolve"
         (format auth/github-url "hive-agi" "bb-depsolve"))))
