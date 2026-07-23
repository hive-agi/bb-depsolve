(ns bb-depsolve.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [bb-depsolve.core :as core]
            [bb-depsolve.version :as v]
            [hive-dsl.result :as r]))

(def ^:private metadata
  "<metadata><versioning><latest>0.1.1</latest><versions>
     <version>0.1.1</version><version>0.1.3-rc1</version>
     <version>0.1.2</version><version>0.1.3</version>
   </versions></versioning></metadata>")

(deftest maven-metadata-test
  (testing "URL is canonical even when the configured base has trailing slashes"
    (is (= "https://registry.example/maven/io/github/hive-agi/hive-carto/maven-metadata.xml"
           (v/maven-metadata-url "https://registry.example/maven///"
                                 "io.github.hive-agi" "hive-carto"))))
  (testing "all published versions are parsed in registry order"
    (is (= ["0.1.1" "0.1.3-rc1" "0.1.2" "0.1.3"]
           (v/parse-maven-metadata-versions metadata))))
  (testing "max version wins over stale metadata <latest>"
    (is (= "0.1.3" (v/latest-published-version metadata {:allow-pre? false}))))
  (testing "malformed or empty metadata is total"
    (is (= [] (v/parse-maven-metadata-versions "not xml")))
    (is (nil? (v/latest-published-version nil {:allow-pre? false})))))

(deftest resolve-mvn-latest-prefers-newest-published-registry-test
  (with-redefs [core/gitea-registry-url (constantly "https://registry.example/maven")
                core/resolve-clojars-latest (fn [& _] (r/ok "0.1.2"))
                core/resolve-maven-latest (fn [& _] (r/err :unexpected/fallback))
                core/resolve-gitea-latest (fn [& _] (r/ok "0.1.3"))]
    (is (= {:ok "0.1.3"}
           (core/resolve-mvn-latest 'io.github.hive-agi/hive-carto false)))))

(deftest resolve-mvn-latest-tolerates-one-registry-failure-test
  (with-redefs [core/gitea-registry-url (constantly "https://registry.example/maven")
                core/resolve-clojars-latest (fn [& _] (r/ok "0.1.2"))
                core/resolve-maven-latest (fn [& _] (r/err :unexpected/fallback))
                core/resolve-gitea-latest (fn [& _] (r/err :io/unavailable))]
    (is (= {:ok "0.1.2"}
           (core/resolve-mvn-latest 'io.github.hive-agi/hive-carto false)))))
