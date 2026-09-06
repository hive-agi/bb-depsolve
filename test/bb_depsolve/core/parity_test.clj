(ns bb-depsolve.core.parity-test
  "Registry parity: a public lib whose private registry is ahead of the public
   one is a forge-sync bug, because a public consumer can never follow it."
  (:require [babashka.process :as proc]
            [bb-depsolve.core.parity :as parity]
            [clojure.test :refer [deftest is testing]]))

(def ^:private lib 'io.github.acme/widget)

(defn- clojars [v] {:id "clojars" :url "https://repo.clojars.org" :public? true :version v})
(defn- gitea [v] {:id "hive-gitea" :url "https://forge.example/m2" :public? false :version v})

(deftest a-public-lib-with-a-private-registry-ahead-is-the-finding
  (testing "declared public, private ahead"
    (is (= {:lib lib :kind :private-ahead :publish :clojars
            :public (clojars "0.1.0") :private (gitea "0.1.1")}
           (parity/classify lib {:publish :clojars} [(clojars "0.1.0") (gitea "0.1.1")]))))
  (testing "declared public, never published publicly"
    (is (= :private-ahead
           (:kind (parity/classify lib {:publish :clojars} [(gitea "0.1.1")])))))
  (testing "hosted on GitHub: public whatever version.edn says, and the hosting is recorded"
    (is (= {:kind :private-ahead :publish :none :hosting :github}
           (select-keys (parity/classify lib {:publish :none :hosting :github}
                                         [(clojars "0.1.0") (gitea "0.1.1")])
                        [:kind :publish :hosting])))
    (is (= :private-ahead
           (:kind (parity/classify lib {:publish :gitea :hosting :github}
                                   [(clojars "0.1.0") (gitea "0.1.1")])))))
  (testing "no declared target counts as public once a public registry lists it"
    (is (= :private-ahead
           (:kind (parity/classify lib {} [(clojars "0.1.0") (gitea "0.1.1")])))))
  (testing "the registries agree"
    (is (nil? (parity/classify lib {:publish :clojars} [(clojars "0.1.1") (gitea "0.1.1")])))
    (is (nil? (parity/classify lib {:publish :clojars} [(clojars "0.1.2") (gitea "0.1.1")]))
        "public ahead of private is not a consumer's problem")
    (is (nil? (parity/classify lib {:publish :clojars} [(clojars "0.1.2")])))
    (is (nil? (parity/classify lib {:publish :gitea :hosting :github} [(clojars "0.1.1") (gitea "0.1.1")]))
        "a GitHub-hosted lib on a public registry is the intended state, whatever it declares")))

(deftest the-other-two-kinds-are-declaration-mismatches
  (is (= :publicly-leaked
         (:kind (parity/classify lib {:publish :gitea :hosting :private} [(clojars "0.1.0") (gitea "0.1.1")])))
      "a private lib on a public registry")
  (is (nil? (parity/classify lib {:publish :gitea :hosting :private} [(gitea "0.1.1")]))
      "a private lib only on the private registry is the intended state")
  (is (nil? (parity/classify lib {} [(gitea "0.1.1")]))
      "an unknown lib only on the private registry is presumed private")
  (is (= :declared-none
         (:kind (parity/classify lib {:publish :none :hosting :private} [(gitea "0.1.1")])))
      "declared unpublished, yet published")
  (is (= :declared-none
         (:kind (parity/classify lib {:publish :none} [(clojars "0.1.0")])))
      "with no hosting evidence the declaration stands")
  (is (nil? (parity/classify lib {:publish :none} []))))

(deftest a-registry-that-did-not-answer-certifies-nothing
  (let [clojars-unread {:id "clojars" :url "https://repo.clojars.org" :public? true
                        :error :io/unread :status 503}
        gitea-unread {:id "hive-gitea" :url "https://forge.example/m2" :public? false
                      :error :io/unread :status 401}]
    (testing "public unread, private known: NOT private-ahead, whatever the private version"
      (is (= {:kind :unread :unread [clojars-unread] :public nil :private (gitea "0.1.1")}
             (select-keys (parity/classify lib {:publish :clojars} [(gitea "0.1.1")] [clojars-unread])
                          [:kind :unread :public :private]))))
    (testing "public known, private unread: nothing to certify either way"
      (is (= :unread
             (:kind (parity/classify lib {:publish :clojars} [(clojars "0.1.0")] [gitea-unread])))))
    (testing "a definite private-ahead from the registries that answered still wins"
      (is (= :private-ahead
             (:kind (parity/classify lib {:publish :clojars} [(clojars "0.1.0") (gitea "0.1.1")] [gitea-unread])))))
    (testing "nothing answered at all"
      (is (= :unread (:kind (parity/classify lib {:publish :clojars} [] [clojars-unread gitea-unread])))))
    (testing "an unread finding blocks, and classify-all reads it off the resolved lib"
      (let [findings (parity/classify-all {'io.github.acme/x {:mvn-unread [clojars-unread]}}
                                          {'io.github.acme/x {:publish :clojars}})]
        (is (= [:unread] (mapv :kind findings)))
        (is (parity/blocking? findings))))))

(deftest findings-cover-every-resolved-lib-and-only-private-ahead-blocks
  (let [resolved {'io.github.acme/a {:mvn-version "0.1.1"
                                     :mvn-by-registry [(clojars "0.1.0") (gitea "0.1.1")]}
                  'io.github.acme/b {:mvn-version "0.1.1"
                                     :mvn-by-registry [(clojars "0.1.1") (gitea "0.1.1")]}
                  'io.github.acme/c {:mvn-version "0.1.1"
                                     :mvn-by-registry [(gitea "0.1.1")]}
                  'io.github.acme/d {:tag "v0.1.0" :sha "abc1234"}}
        evidence-of {'io.github.acme/a {:publish :clojars}
                     'io.github.acme/b {:publish :clojars}
                     'io.github.acme/c {:publish :none :hosting :private}}]
    (is (= [['io.github.acme/a :private-ahead] ['io.github.acme/c :declared-none]]
           (mapv (juxt :lib :kind) (parity/classify-all resolved evidence-of))))
    (is (parity/blocking? (parity/classify-all resolved evidence-of)))
    (is (not (parity/blocking? [{:kind :declared-none}])))))

(deftest the-remedy-names-the-github-remote
  (let [root (str (java.nio.file.Files/createTempDirectory
                   "depsolve-parity" (into-array java.nio.file.attribute.FileAttribute [])))
        dir (str root "/widget")]
    (.mkdirs (java.io.File. dir))
    (proc/sh ["git" "-C" dir "init" "-q"])
    (is (= {:publish nil} (parity/evidence root lib))
        "a checkout with no version.edn and no remotes says nothing")
    (proc/sh ["git" "-C" dir "remote" "add" "origin" "git@gitea.example:acme/widget.git"])
    (is (nil? (parity/github-remote dir)) "a private forge remote is not the one to push")
    (is (= :private (:hosting (parity/evidence root lib))))
    (proc/sh ["git" "-C" dir "remote" "add" "github" "git@github.com:acme/widget.git"])
    (spit (str dir "/version.edn") "{:lib io.github.acme/widget :publish :none}")
    (is (= "github" (parity/github-remote dir)))
    (is (= {:publish :none :hosting :github} (parity/evidence root lib))
        "version.edn and the remotes are both read")
    (is (= dir (parity/lib-dir root lib)) "the sibling checkout is found by artifact id")
    (is (nil? (parity/lib-dir root 'io.github.acme/absent)))
    (is (= {} (parity/evidence root 'io.github.acme/absent)))))
