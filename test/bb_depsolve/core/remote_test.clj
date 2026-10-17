(ns bb-depsolve.core.remote-test
  "Tests for bb-depsolve.core.remote: the pure override normalization under a
   trifecta, and the {repo-dir => provider} projection against throwaway local
   repos whose remotes are only configured, never contacted."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.core.remote :as remote]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

;; =============================================================================
;; Pure: override normalization
;; =============================================================================

(tri/deftrifecta normalize-overrides-trifecta
  bb-depsolve.core.remote/normalize-overrides
  {:golden-path "test/golden/bb-depsolve/remote-overrides.edn"
   :cases {:strings   {"Git.Example.Org" :gitea}
           :keywords  {:code.corp.example :gitlab}
           :string-provider {"forge.example.org" "gitea"}
           :unknown-provider {"forge.example.org" :sourcehut}
           :blank-host {"" :github}
           :not-a-map [["forge.example.org" :gitea]]
           :nil nil}
   :gen (gen/one-of [(gen/return nil)
                     (gen/map gen/string-alphanumeric
                              (gen/elements [:github :gitea :gitlab :bitbucket :other :nope]))])
   :pred map?
   :num-tests 50
   :mutations [["keeps-everything" (fn [raw] (if (map? raw) raw {}))]]})

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- git! [dir & args]
  (let [r (apply git/git dir args)]
    (when-not (zero? (:exit r))
      (throw (ex-info (str "git " (pr-str args) " failed") r)))
    r))

(defn- repo!
  "A git repo named NAME under ROOT with a VERSION file and the given
   REMOTES ({remote-name url}), in insertion order."
  [root name remotes]
  (let [dir (str (fs/path root name))]
    (fs/create-dirs dir)
    (git! dir "init" "--initial-branch=main")
    (spit (str (fs/path dir "VERSION")) "0.1.0\n")
    (doseq [[n url] remotes]
      (git! dir "remote" "add" n url))
    dir))

(defn- workspace []
  (str (fs/create-temp-dir {:prefix "bb-depsolve-remote"})))

;; =============================================================================
;; git-first-remote-url
;; =============================================================================

(deftest git-first-remote-url-test
  (let [root (workspace)]
    (testing "no remote -> nil"
      (is (nil? (git/git-first-remote-url (repo! root "bare" [])))))
    (testing "origin wins over an earlier remote"
      (is (= "git@github.com:acme/a.git"
             (git/git-first-remote-url
              (repo! root "multi" [["upstream" "https://gitlab.com/x/a.git"]
                                   ["origin" "git@github.com:acme/a.git"]])))))
    (testing "without origin, the first listed remote"
      (is (= "https://bitbucket.org/acme/b.git"
             (git/git-first-remote-url
              (repo! root "noorigin" [["mirror" "https://bitbucket.org/acme/b.git"]])))))))

;; =============================================================================
;; classify-repos (ports injected)
;; =============================================================================

(deftest classify-repos-test
  (let [urls {"a" "https://github.com/acme/a.git"
              "b" "ssh://git@forge.example.org/acme/b.git"
              "c" nil}
        got  (remote/classify-repos ["a" "b" "c"] urls {"forge.example.org" :gitea})]
    (is (= {"a" :github "b" :gitea "c" :other} got))
    (testing "override outranks the built-in host"
      (is (= {"a" :other}
             (remote/classify-repos ["a"] urls {"github.com" :other}))))))

;; =============================================================================
;; Config + discovery projection
;; =============================================================================

(deftest read-overrides-test
  (let [root (workspace)]
    (testing "no config file"
      (is (= {} (remote/read-overrides root))))
    (testing "config is normalized"
      (spit (remote/config-path root)
            "{:remote-providers {\"Git.Example.Org\" :gitea \"bad\" :nope}}")
      (is (= {"git.example.org" :gitea} (remote/read-overrides root))))
    (testing "unreadable config degrades to no overrides"
      (spit (remote/config-path root) "{:remote-providers")
      (is (= {} (remote/read-overrides root))))))

(deftest remote-providers-test
  (let [root (workspace)
        gh   (repo! root "gh" [["origin" "git@github.com:acme/gh.git"]])
        self (repo! root "self" [["origin" "https://git.example.org/acme/self.git"]])
        none (repo! root "none" [])]
    (testing "built-in heuristics only"
      (is (= {gh :github self :other none :other}
             (remote/remote-providers root))))
    (testing ".bb-depsolve.edn :remote-providers classifies the self-hosted forge"
      (spit (remote/config-path root)
            "{:remote-providers {\"git.example.org\" :gitea}}")
      (is (= {gh :github self :gitea none :other}
             (remote/remote-providers root))))
    (testing "skip dirs are honoured"
      (is (= {gh :github none :other}
             (remote/remote-providers root #{"self"}))))))
