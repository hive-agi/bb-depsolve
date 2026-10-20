(ns bb-depsolve.core.push-policy-test
  "The push policy behind push-all --remotes / --skip-remote: the pure glob,
   parse and decision functions under trifectas, and the {repo-dir => decision}
   projection against throwaway local repos whose remotes are only configured,
   never contacted."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.core.remote :as remote]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

;; =============================================================================
;; Pure: decision over (policy, url)
;; =============================================================================

(defn- decide
  "One trifecta subject input: a policy, a remote URL and host overrides."
  [{:keys [policy url overrides]}]
  (remote/push-decision policy (remote/remote-target url (or overrides {}))))

(defn- decision? [d]
  (and (boolean? (:push? d))
       (contains? #{:allowed :excluded :not-included} (:reason d))))

(def ^:private gitea-url "https://gitea.example.org/acme/repo.git")
(def ^:private github-url "git@github.com:acme/repo.git")

(tri/deftrifecta push-decision-trifecta
  decide
  {:golden-path "test/golden/bb-depsolve/push-decision.edn"
   :cases {:empty-policy {:policy {:include [] :exclude []} :url gitea-url}
           :excluded-by-provider {:policy {:include [] :exclude ["gitea"]} :url gitea-url}
           :excluded-by-host-glob {:policy {:include [] :exclude ["gitea.*"]} :url gitea-url}
           :excluded-by-url-glob {:policy {:include [] :exclude ["*://gitea.example.org/*"]}
                                  :url gitea-url}
           :other-forge-passes {:policy {:include [] :exclude ["gitea.*"]} :url github-url}
           :included {:policy {:include ["github"] :exclude []} :url github-url}
           :not-included {:policy {:include ["github"] :exclude []} :url gitea-url}
           :exclude-outranks-include {:policy {:include ["*"] :exclude ["gitea"]} :url gitea-url}
           :override-classifies {:policy {:include [] :exclude ["gitea"]}
                                 :url "https://forge.corp.example/acme/repo.git"
                                 :overrides {"forge.corp.example" :gitea}}}
   :gen (gen/fmap (fn [[inc exc url]] {:policy {:include inc :exclude exc} :url url})
                  (gen/tuple (gen/vector (gen/elements ["github" "gitea.*" "*"]) 0 2)
                             (gen/vector (gen/elements ["github" "gitea.*" "gitlab"]) 0 2)
                             (gen/elements [gitea-url github-url "not a remote"])))
   :pred decision?
   :num-tests 60
   :mutations [["ignores-the-policy" (fn [_] {:push? true :reason :allowed :pattern nil})]
               ["include-outranks-exclude"
                (fn [{:keys [policy url]}]
                  (if (seq (:include policy))
                    {:push? true :reason :allowed :pattern nil}
                    (remote/push-decision policy (remote/remote-target url {}))))]]})

(defn- glob-match
  "One trifecta subject input: a glob pattern and the text it is matched on."
  [{:keys [pattern text]}]
  (remote/glob-match? pattern text))

(tri/deftrifecta glob-match-trifecta
  glob-match
  {:golden-path "test/golden/bb-depsolve/glob-match.edn"
   :cases {:literal {:pattern "gitea" :text "gitea"}
           :literal-miss {:pattern "gitea" :text "gitea.com"}
           :trailing-star {:pattern "gitea.*" :text "gitea.example.org"}
           :leading-star {:pattern "*.corp.example" :text "git.corp.example"}
           :middle-star {:pattern "git*.org" :text "gitea.example.org"}
           :question {:pattern "gite?" :text "gitea"}
           :case-insensitive {:pattern "GitHub.com" :text "github.com"}
           :star-spans-slashes {:pattern "*://x/*" :text "https://x/a/b"}
           :backtrack {:pattern "*a*b" :text "aXabYb"}
           :star-alone {:pattern "*" :text "anything"}
           :empty-pattern {:pattern "" :text "x"}}
   :gen (gen/fmap (fn [[p t]] {:pattern p :text t})
                  (gen/tuple (gen/elements ["*" "a*" "*a" "a?c" "abc" ""])
                             (gen/elements ["abc" "a" "" "xabc"])))
   :pred boolean?
   :num-tests 60
   :mutations [["star-matches-nothing"
                (fn [{:keys [pattern text]}] (= pattern text))]]})

(deftest glob-match-properties-test
  (testing "a star-free pattern is plain case-insensitive equality"
    (is (remote/glob-match? "abc" "ABC"))
    (is (not (remote/glob-match? "abc" "abcd"))))
  (testing "the lone star matches everything, including nothing"
    (is (remote/glob-match? "*" ""))
    (is (remote/glob-match? "**" "abc")))
  (testing "regex metacharacters are literal"
    (is (remote/glob-match? "a.b" "a.b"))
    (is (not (remote/glob-match? "a.b" "axb")))))

(deftest parse-patterns-test
  (is (= ["gitea.*" "github"] (remote/parse-patterns "gitea.*, github")))
  (is (= ["gitea.*"] (remote/parse-patterns "gitea.*,,")))
  (is (= ["a" "b" "c"] (remote/parse-patterns [:a "b" 'c " "])))
  (is (= ["*"] (remote/parse-patterns true)))
  (is (= [] (remote/parse-patterns nil)))
  (is (= [] (remote/parse-patterns 42))))

(deftest normalize-push-policy-test
  (is (= {:include [] :exclude []} (remote/normalize-push-policy nil)))
  (is (= {:include [] :exclude []} (remote/normalize-push-policy [:include "x"])))
  (is (= {:include ["github"] :exclude ["gitea.*" "gitlab"]}
         (remote/normalize-push-policy {:include ["github"] :exclude "gitea.*,gitlab"}))))

(deftest resolve-push-policy-test
  (let [config {:include ["github"] :exclude ["gitea"]}]
    (testing "no flags: the config stands"
      (is (= {:include ["github"] :exclude ["gitea"]}
             (remote/resolve-push-policy config {}))))
    (testing "a flag replaces only its own side"
      (is (= {:include ["github"] :exclude ["gitlab"]}
             (remote/resolve-push-policy config {:skip-remote "gitlab"})))
      (is (= {:include ["bitbucket"] :exclude ["gitea"]}
             (remote/resolve-push-policy config {:remotes "bitbucket"}))))
    (testing "the bare --skip-remote excludes everything"
      (is (= ["*"] (:exclude (remote/resolve-push-policy {} {:skip-remote true})))))))

;; =============================================================================
;; Projection over repos (ports injected) + config
;; =============================================================================

(deftest push-decisions-test
  (let [urls   {"gh" github-url
                "ge" gitea-url
                "none" nil}
        policy {:include [] :exclude ["gitea"]}
        got    (remote/push-decisions ["gh" "ge" "none"] urls {} policy)]
    (is (true? (:push? (get got "gh"))))
    (is (= {:push? false :reason :excluded :pattern "gitea"} (get got "ge")))
    (testing "a remoteless repo is never excluded; the push plan skips it itself"
      (is (= {:push? true :reason :no-remote :pattern nil} (get got "none"))))))

(deftest workspace-push-policy-test
  (let [root (str (fs/create-temp-dir {:prefix "bb-depsolve-policy"}))]
    (testing "no config file: empty policy"
      (is (= {:policy {:include [] :exclude []} :overrides {}}
             (remote/workspace-push-policy root {}))))
    (testing ".bb-depsolve.edn :push-policy is the fallback, overrides ride along"
      (spit (remote/config-path root)
            (str "{:remote-providers {\"forge.corp.example\" :gitea}"
                 " :push-policy {:exclude [\"gitea\"]}}"))
      (is (= {:policy {:include [] :exclude ["gitea"]}
              :overrides {"forge.corp.example" :gitea}}
             (remote/workspace-push-policy root {}))))
    (testing "the flag outranks the config on its own side"
      (is (= ["gitlab"]
             (get-in (remote/workspace-push-policy root {:skip-remote "gitlab"})
                     [:policy :exclude]))))
    (testing "an unreadable config degrades to the empty policy"
      (spit (remote/config-path root) "{:push-policy")
      (is (= {:include [] :exclude []}
             (:policy (remote/workspace-push-policy root {})))))))

(deftest remote-host-test
  (is (= "github.com" (git/remote-host "git@github.com:acme/a.git")))
  (is (= "git.example.org" (git/remote-host "ssh://git@Git.Example.Org/a.git")))
  (is (nil? (git/remote-host "not a remote")))
  (is (nil? (git/remote-host nil))))

(deftest hive-workspace-shape-test
  (testing "the card's DoD: gitea remotes are excluded, github ones still push"
    (let [urls {"hive-crawl" "https://gitea.example.org/hive/hive-crawl.git"
                "hive-ingestor" "https://gitea.example.org/hive/hive-ingestor.git"
                "hive-agent" "ssh://git@gitea.example.org/hive/hive-agent.git"
                "bb-depsolve" "git@github.com:hive-agi/bb-depsolve.git"}
          got  (remote/push-decisions (keys urls) urls {} {:include [] :exclude ["gitea.*"]})]
      (is (= #{"hive-crawl" "hive-ingestor" "hive-agent"}
             (set (keep (fn [[k v]] (when-not (:push? v) k)) got))))
      (is (:push? (get got "bb-depsolve"))))))
