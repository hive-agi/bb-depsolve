(ns bb-depsolve.core.resolve-test
  "Tests for registry resolution across Clojars, Maven Central and Gitea."
  (:require [bb-depsolve.core.auth :as auth]
            [bb-depsolve.core.resolve.registries :as registries]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [babashka.http-client :as http]))

(def ^:private clojars "https://repo.clojars.org")

(def ^:private central "https://repo1.maven.org/maven2")

(def ^:private private-url "https://registry.example/maven")

(defn- answering
  "Stub for read-registry-latest: ANSWERS maps a registry base-url to its
   read ({:version v} / {:absent true} / {:unread {...}}), or to a fn of
   allow-pre?. A registry left out answers absent."
  [answers]
  (fn [base-url _ _ _ allow-pre?]
    (let [answer (get answers base-url {:absent true})]
      (if (fn? answer) (answer allow-pre?) answer))))

(defmacro ^:private with-registries
  "Run BODY with the private registry at private-url and every registry read
   answered by ANSWERS."
  [answers & body]
  `(with-redefs [auth/gitea-registry-url (constantly private-url)
                 auth/private-registry (constantly nil)
                 registries/read-registry-latest (answering ~answers)]
     ~@body))

(deftest resolve-mvn-latest-prefers-newest-published-registry-test
  (with-registries {clojars {:version "0.1.2"} private-url {:version "0.1.3"}}
    (is (= {:ok "0.1.3"}
           (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto false)))))

(deftest resolve-mvn-latest-tolerates-one-registry-failure-test
  (with-registries {clojars {:version "0.1.2"} private-url {:unread {:status 503}}}
    (is (= {:ok "0.1.2"}
           (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto false))
        "the resolver-wide view is the newest among the registries that answered")
    (is (= {:versions [{:id "clojars" :url clojars :public? true :version "0.1.2"}]
            :unread [{:id "registry.example" :url private-url :public? false
                      :error :io/unread :status 503}]}
           (registries/resolve-mvn-reads 'io.github.hive-agi/hive-carto false))
        "and the registry that did not answer is reported, not dropped")))

(deftest resolve-mvn-by-registry-keeps-registries-apart-test
  (with-registries {clojars {:version "0.1.2"} private-url {:version "0.1.3"}}
    (is (= [{:id "clojars" :url clojars :public? true :version "0.1.2"}
            {:id "registry.example" :url private-url :public? false :version "0.1.3"}]
           (:ok (registries/resolve-mvn-by-registry 'io.github.hive-agi/hive-carto false)))
        "one entry per registry, sorted by id; the private registry is labelled by
         its host when it has no repository id, and is never public")))

(deftest central-is-consulted-only-when-clojars-answers-absent-test
  (testing "absent on Clojars, present on Central"
    (with-registries {central {:version "1.0.0"}}
      (is (= [{:id "central" :url central :public? true :version "1.0.0"}]
             (:ok (registries/resolve-mvn-by-registry 'org.example/lib false))))))
  (testing "a Clojars that did not answer is unread, and Central is not asked to paper over it"
    (with-registries {clojars {:unread {:status nil :message "connect timed out"}}
                      central (fn [_] (throw (ex-info "central must not be consulted" {})))}
      (is (= {:versions []
              :unread [{:id "clojars" :url clojars :public? true :error :io/unread
                        :message "connect timed out"}]}
             (registries/resolve-mvn-reads 'org.example/lib false)))
      (is (= :io/registry-unread
             (:error (registries/resolve-mvn-by-registry 'org.example/lib false)))
          "nothing answered, but the read was blind: not the same as unpublished"))))

(deftest resolve-mvn-latest-errs-when-every-registry-fails-test
  (testing "every registry answered, none lists it"
    (with-registries {}
      (is (= :io/no-published-version
             (:error (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto false))))))
  (testing "no registry answered"
    (with-registries {clojars {:unread {:status 503}} private-url {:unread {:status 401}}}
      (is (= :io/registry-unread
             (:error (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto false))))))) 

(deftest resolve-mvn-latest-filters-pre-releases-unless-asked-test
  (with-registries {clojars (fn [allow-pre?] (if allow-pre? {:version "0.2.0-rc1"} {:absent true}))}
    (is (= :io/no-published-version
           (:error (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto false)))
        "a pre-release is not an acceptable published version by default")
    (is (= {:ok "0.2.0-rc1"}
           (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto true)))))

(deftest read-registry-latest-keeps-absent-apart-from-unread-test
  (let [metadata (str "<metadata><versioning><versions>"
                      "<version>0.1.0</version><version>0.2.0-rc1</version>"
                      "</versions></versioning></metadata>")
        read (fn [response]
               (with-redefs [http/get (fn [& _] (if (fn? response) (response) response))]
                 (registries/read-registry-latest clojars :clojars "acme" "widget" false)))]
    (is (= {:version "0.1.0"} (read {:status 200 :body metadata}))
        "200 with an acceptable version")
    (is (= {:absent true} (read {:status 200 :body "<metadata><versioning><versions><version>0.2.0-rc1</version></versions></versioning></metadata>"}))
        "200 with only a pre-release: answered, nothing acceptable")
    (is (= {:absent true} (read {:status 404 :body ""}))
        "404 is an answer")
    (is (= {:unread {:status 503}} (read {:status 503 :body ""}))
        "5xx is not an answer")
    (is (= {:unread {:status 401}} (read {:status 401 :body ""}))
        "an auth refusal is a blind read, not an absence")
    (is (= {:unread {:status nil :message "connect timed out"}}
           (read (fn [] (throw (ex-info "connect timed out" {})))))
        "a transport failure carries its message")))
