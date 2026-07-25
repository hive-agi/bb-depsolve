(ns bb-depsolve.core.resolve-test
  "Tests for registry resolution across Clojars, Maven Central and Gitea."
  (:require [bb-depsolve.core.auth :as auth]
            [bb-depsolve.core.resolve.registries :as registries]
            [clojure.test :refer [deftest is]]
            [hive-dsl.result :as r]))

(deftest resolve-mvn-latest-prefers-newest-published-registry-test
  (with-redefs [auth/gitea-registry-url (constantly "https://registry.example/maven")
                registries/resolve-clojars-latest (fn [& _] (r/ok "0.1.2"))
                registries/resolve-maven-latest (fn [& _] (r/err :unexpected/fallback))
                registries/resolve-gitea-latest (fn [& _] (r/ok "0.1.3"))
                registries/resolve-cached-maven-latest (fn [& _] (r/err :io/maven-cache))]
    (is (= {:ok "0.1.3"}
           (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto false)))))

(deftest resolve-mvn-latest-tolerates-one-registry-failure-test
  (with-redefs [auth/gitea-registry-url (constantly "https://registry.example/maven")
                registries/resolve-clojars-latest (fn [& _] (r/ok "0.1.2"))
                registries/resolve-maven-latest (fn [& _] (r/err :unexpected/fallback))
                registries/resolve-gitea-latest (fn [& _] (r/err :io/unavailable))
                registries/resolve-cached-maven-latest (fn [& _] (r/err :io/maven-cache))]
    (is (= {:ok "0.1.2"}
           (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto false)))))

(deftest resolve-mvn-latest-consults-the-local-maven-cache-test
  (with-redefs [auth/gitea-registry-url (constantly "https://registry.example/maven")
                registries/resolve-clojars-latest (fn [& _] (r/ok "0.1.2"))
                registries/resolve-maven-latest (fn [& _] (r/err :unexpected/fallback))
                registries/resolve-gitea-latest (fn [& _] (r/err :io/unavailable))
                registries/resolve-cached-maven-latest (fn [& _] (r/ok "0.1.4"))]
    (is (= {:ok "0.1.4"}
           (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto false))
        "cached REMOTE metadata is an offline view of a private registry")))

(deftest resolve-mvn-latest-errs-when-every-registry-fails-test
  (with-redefs [auth/gitea-registry-url (constantly nil)
                registries/resolve-clojars-latest (fn [& _] (r/err :io/unavailable))
                registries/resolve-maven-latest (fn [& _] (r/err :io/unavailable))]
    (let [{:keys [error]} (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto false)]
      (is (= :io/no-published-version error))))) 

(deftest resolve-mvn-latest-filters-pre-releases-unless-asked-test
  (with-redefs [auth/gitea-registry-url (constantly nil)
                registries/resolve-clojars-latest (fn [& _] (r/ok "0.2.0-rc1"))
                registries/resolve-maven-latest (fn [& _] (r/err :unexpected/fallback))]
    (is (= :io/no-published-version
           (:error (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto false)))
        "a pre-release is not an acceptable published version by default")
    (is (= {:ok "0.2.0-rc1"}
           (registries/resolve-mvn-latest 'io.github.hive-agi/hive-carto true)))))
