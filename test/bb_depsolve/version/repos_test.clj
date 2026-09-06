(ns bb-depsolve.version.repos-test
  "A resolution is projected through the CONSUMER's registries before it
   becomes a pin: a public registry is always reachable, a private one only
   when the consumer declares it."
  (:require [bb-depsolve.version.repos :as repos]
            [clojure.test :refer [deftest is testing]]))

(def ^:private gitea
  "https://forge.example/api/packages/acme/maven")

(def ^:private clojars-0-1-0
  {:id "clojars" :url "https://repo.clojars.org" :public? true :version "0.1.0"})

(def ^:private gitea-0-1-1
  {:id "hive-gitea" :url gitea :public? false :version "0.1.1"})

(def ^:private declares-gitea
  [{:id "hive-gitea" :url (str gitea "/")}])

(deftest declared-repos-reads-what-the-file-declares
  (testing "top level and aliases"
    (is (= #{"hive-gitea" "alias-forge"}
           (set (map :id (repos/declared-repos
                          (str "{:mvn/repos {\"hive-gitea\" {:url \"" gitea "\"}}"
                               " :aliases {:build {:mvn/repos {\"alias-forge\" {:url \"https://x/m2\"}}}}}")))))))
  (testing "a bb.edn whose tasks carry reader forms EDN refuses still declares its repos"
    (is (= [{:id "hive-gitea" :url gitea}]
           (repos/declared-repos
            (str "{:mvn/repos {\"hive-gitea\" {:url \"" gitea "\"}}"
                 " :tasks {t {:task (run #(println %) @(atom 1))}}}")))))
  (testing "totality"
    (is (= [] (repos/declared-repos "{:deps {}}")))
    (is (= [] (repos/declared-repos "{not edn")))
    (is (= [] (repos/declared-repos nil)))))

(deftest reachability-is-public-or-declared
  (is (repos/reachable? [] clojars-0-1-0) "public needs no declaration")
  (is (not (repos/reachable? [] gitea-0-1-1)) "private needs one")
  (is (repos/reachable? declares-gitea gitea-0-1-1) "by url, trailing slash ignored")
  (is (repos/reachable? [{:id "hive-gitea" :url "https://elsewhere/m2"}] gitea-0-1-1)
      "or by repository id")
  (is (not (repos/reachable? [{:id "other" :url "https://elsewhere/m2"}] gitea-0-1-1))))

(deftest a-lib-is-projected-through-the-consumer-registries
  (let [lib {:mvn-version "0.1.1" :mvn-by-registry [clojars-0-1-0 gitea-0-1-1]}]
    (testing "a public consumer sees the public version and is told what it misses"
      (is (= {:mvn-version "0.1.0" :mvn-source "clojars"
              :mvn-unreachable [gitea-0-1-1]
              :mvn-by-registry [clojars-0-1-0 gitea-0-1-1]}
             (repos/project-lib lib []))))
    (testing "a consumer declaring the private registry sees its newest"
      (is (= {:mvn-version "0.1.1" :mvn-source "hive-gitea"
              :mvn-by-registry [clojars-0-1-0 gitea-0-1-1]}
             (repos/project-lib lib declares-gitea))))
    (testing "an older unreachable version is not worth mentioning"
      (is (nil? (:mvn-unreachable
                 (repos/project-lib {:mvn-version "0.1.0"
                                     :mvn-by-registry [{:id "clojars" :public? true :version "0.1.0"}
                                                       {:id "hive-gitea" :public? false :version "0.0.9"}]}
                                    [])))))
    (testing "nothing reachable and no git coordinate: nothing to pin"
      (is (nil? (repos/project-lib {:mvn-version "0.1.1" :mvn-by-registry [gitea-0-1-1]} []))))
    (testing "nothing reachable but a git coordinate: the tag survives, the version goes"
      (is (= {:tag "v0.1.1" :sha "abc1234" :mvn-by-registry [gitea-0-1-1]
              :mvn-unreachable [gitea-0-1-1]}
             (repos/project-lib {:tag "v0.1.1" :sha "abc1234" :mvn-version "0.1.1"
                                 :mvn-by-registry [gitea-0-1-1]}
                                []))))
    (testing "a lib resolved without registry detail is passed through"
      (is (= {:mvn-version "9.9.9"} (repos/project-lib {:mvn-version "9.9.9"} []))))))

(deftest an-unread-reachable-registry-makes-the-projection-uncertain
  (let [clojars-unread {:id "clojars" :url "https://repo.clojars.org" :public? true
                        :error :io/unread :status 503}
        gitea-unread {:id "hive-gitea" :url gitea :public? false :error :io/unread :status 401}]
    (testing "public registry unread: no version is chosen for anyone"
      (let [lib {:mvn-version "0.1.1" :mvn-by-registry [gitea-0-1-1] :mvn-unread [clojars-unread]}]
        (is (nil? (repos/project-lib lib []))
            "a public consumer has nothing certain to pin")
        (is (nil? (repos/project-lib lib declares-gitea))
            "even a consumer that reaches the private version holds, since Clojars may hold a newer one")))
    (testing "private registry unread: only consumers that reach it are held"
      (let [lib {:mvn-version "0.1.0" :mvn-by-registry [clojars-0-1-0] :mvn-unread [gitea-unread]}]
        (is (= "0.1.0" (:mvn-version (repos/project-lib lib [])))
            "a public consumer never reaches the private registry, so its read does not matter")
        (is (nil? (repos/project-lib lib declares-gitea))
            "a consumer declaring it has nothing certain to pin")))
    (testing "a git coordinate survives an uncertain Maven read, and names the uncertainty"
      (is (= {:tag "v0.1.1" :sha "abc1234" :mvn-unread [clojars-unread] :mvn-uncertain [clojars-unread]}
             (repos/project-lib {:tag "v0.1.1" :sha "abc1234" :mvn-version "0.1.1" :mvn-unread [clojars-unread]} []))
          "the tag can still be synced; the Maven version is neither chosen nor carried"))))

(deftest a-resolution-is-projected-lib-by-lib
  (let [resolved {'acme/a {:mvn-version "0.1.1" :mvn-by-registry [clojars-0-1-0 gitea-0-1-1]}
                  'acme/b {:mvn-version "0.1.1" :mvn-by-registry [gitea-0-1-1]}}]
    (is (= {'acme/a "0.1.0"}
           (update-vals (repos/project-resolved resolved []) :mvn-version))
        "the lib nothing can be pinned for drops out")
    (is (= {'acme/a "0.1.1" 'acme/b "0.1.1"}
           (update-vals (repos/project-resolved resolved declares-gitea) :mvn-version)))))

(deftest withheld-names-pinned-libs-sync-will-not-move
  (let [clojars-unread {:id "clojars" :url "https://repo.clojars.org" :public? true
                        :error :io/unread :status 503}
        gitea-unread {:id "hive-gitea" :url gitea :public? false :error :io/unread :status 401}
        resolved {'acme/a {:mvn-version "0.1.1" :mvn-by-registry [clojars-0-1-0 gitea-0-1-1]}
                  'acme/b {:mvn-version "0.1.1" :mvn-by-registry [gitea-0-1-1]}
                  'acme/c {:mvn-version "0.1.1" :mvn-by-registry [gitea-0-1-1]}
                  'acme/d {:mvn-version "0.1.1" :mvn-by-registry [gitea-0-1-1] :mvn-unread [clojars-unread]}
                  'acme/e {:mvn-version "0.1.0" :mvn-by-registry [clojars-0-1-0] :mvn-unread [gitea-unread]}}]
    (is (= [{:lib 'acme/b :reason :unreachable :versions [gitea-0-1-1]}
            {:lib 'acme/d :reason :unread :unread [clojars-unread]}]
           (repos/withheld resolved [] ['acme/a 'acme/b 'acme/d 'acme/e]))
        "a is reachable; c is not pinned by this consumer; d's public registry did not
         answer; e's unread registry is private and this consumer does not reach it")
    (is (= [{:lib 'acme/d :reason :unread :unread [clojars-unread]}
            {:lib 'acme/e :reason :unread :unread [gitea-unread]}]
           (repos/withheld resolved declares-gitea ['acme/a 'acme/b 'acme/c 'acme/d 'acme/e]))
        "a consumer declaring the private registry reaches both unread registries")))
