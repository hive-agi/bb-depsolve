(ns bb-depsolve.reach-test
  "An await is satisfied only by a publication the plan's later consumers can
   actually fetch: published anywhere is not published for a public consumer
   when only the private registry has it."
  (:require [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.core.resolve.registries :as registries]
            [bb-depsolve.registry.live :as reg]
            [bb-depsolve.release.port :as p]
            [bb-depsolve.version.api :as v]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(def ^:private weave 'io.github.test/weave)
(def ^:private gitea "https://forge.example/m2")
(def ^:private declares-gitea [{:id "hive-gitea" :url gitea}])

(def ^:private published
  "0.3.1 is public; 0.3.2 exists only privately and as a tag."
  [{:id "clojars" :url "https://repo.clojars.org" :public? true :kind :mvn :version "0.3.1"}
   {:id "hive-gitea" :url gitea :public? false :kind :mvn :version "0.3.2"}
   {:id "git" :public? true :kind :git :version "0.3.2"}])

(defn- registry-of
  [entries]
  (reify p/IArtifactRegistry
    (published? [_ _ version]
      (r/ok (contains? (set (map :version entries)) (v/tag->mvn-version version))))
    (latest-version [_ _]
      (r/ok (last (sort v/version-compare (map :version entries)))))
    (published-versions [_ _]
      (r/ok entries))))

(defn- satisfied?
  [entry]
  (:ok (p/await-satisfied? (registry-of published) entry)))

(deftest a-consumer-is-satisfied-only-through-a-source-it-can-use
  (is (p/satisfies-consumer? published "0.3.1" {:coord :mvn}))
  (is (not (p/satisfies-consumer? published "0.3.2" {:coord :mvn}))
      "a public Maven consumer cannot use the private registry, nor a tag")
  (is (p/satisfies-consumer? published "0.3.2" {:coord :mvn :repos declares-gitea}))
  (is (p/satisfies-consumer? published "0.3.2" {:coord :git}))
  (is (not (p/satisfies-consumer? published "0.3.1" {:coord :git}))
      "a git consumer needs the tag, not a Maven artifact")
  (is (p/satisfies-consumer? [{:id "memory" :public? true :kind :any :version "1.0.0"}]
                             "1.0.0" {:coord :mvn :repos declares-gitea})
      "a source serving both kinds satisfies anyone"))

(deftest an-exact-await-must-be-fetchable-by-every-consumer
  (testing "public consumer, private-only version"
    (is (false? (satisfied? {:lib weave :expect "0.3.2" :reach [{:coord :mvn}]}))))
  (testing "the consumer declares the private registry"
    (is (true? (satisfied? {:lib weave :expect "0.3.2" :reach [{:coord :mvn :repos declares-gitea}]}))))
  (testing "a git consumer, tag form of the expectation"
    (is (true? (satisfied? {:lib weave :expect "v0.3.2" :reach [{:coord :git}]}))))
  (testing "mixed consumers: the weakest decides"
    (is (false? (satisfied? {:lib weave :expect "0.3.2"
                             :reach [{:coord :mvn :repos declares-gitea} {:coord :mvn}]})))
    (is (true? (satisfied? {:lib weave :expect "0.3.1"
                            :reach [{:coord :mvn :repos declares-gitea} {:coord :mvn}]})))))

(deftest an-open-await-takes-any-newer-version-every-consumer-can-fetch
  (is (true? (satisfied? {:lib weave :newer-than "0.3.0" :reach [{:coord :mvn}]}))
      "0.3.1 is public and newer")
  (is (false? (satisfied? {:lib weave :newer-than "0.3.1" :reach [{:coord :mvn}]}))
      "only 0.3.2 is newer, and a public consumer cannot fetch it")
  (is (true? (satisfied? {:lib weave :newer-than "0.3.1" :reach [{:coord :git}]}))))

(deftest without-a-reach-any-publication-counts
  (is (true? (satisfied? {:lib weave :expect "0.3.2"}))
      "the old contract: published anywhere")
  (is (true? (satisfied? {:lib weave :newer-than "0.3.1"}))))

(deftest the-memory-port-serves-every-consumer
  (let [port (p/memory-port {:registry {weave #{"0.3.1"}}})]
    (is (= [{:id "memory" :public? true :kind :any :version "0.3.1"}]
           (:ok (p/published-versions port weave))))
    (is (true? (:ok (p/await-satisfied? port {:lib weave :expect "0.3.1"
                                               :reach [{:coord :mvn} {:coord :git}]}))))))

(deftest the-live-registry-keeps-its-sources-apart
  (with-redefs [resolve/resolve-remote-tags (fn [& _] (r/ok [{:tag "v0.3.2" :sha "abc1234"}]))
                registries/resolve-mvn-versions-by-registry
                (fn [& _] [{:id "clojars" :url "https://repo.clojars.org" :public? true :versions #{"0.3.0" "0.3.1"}}
                           {:id "hive-gitea" :url gitea :public? false :versions #{"0.3.2"}}])]
    (is (= [{:id "git" :public? true :kind :git :version "0.3.2"}
            {:id "clojars" :url "https://repo.clojars.org" :public? true :kind :mvn :version "0.3.0"}
            {:id "clojars" :url "https://repo.clojars.org" :public? true :kind :mvn :version "0.3.1"}
            {:id "hive-gitea" :url gitea :public? false :kind :mvn :version "0.3.2"}]
           (:ok (p/published-versions (reg/live-registry) 'io.github.hive-agi/weave))))
    (is (false? (:ok (p/await-satisfied? (reg/live-registry)
                                         {:lib 'io.github.hive-agi/weave :expect "0.3.2"
                                          :reach [{:coord :mvn}]})))
        "the cascade bug: published on the private registry and tagged, yet a public consumer waits")
    (is (true? (:ok (p/await-satisfied? (reg/live-registry)
                                        {:lib 'io.github.hive-agi/weave :expect "0.3.2"}))))))
