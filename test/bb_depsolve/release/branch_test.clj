(ns bb-depsolve.release.branch-test
  "Tests for bb-depsolve.release.branch against throwaway local repos with a
   bare remote. Every effect lands in a temp dir the test creates."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.release.branch :as branch]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- git! [dir & args]
  (let [r (apply git/git dir args)]
    (when-not (zero? (:exit r))
      (throw (ex-info (str "git " (str/join " " args) " failed") r)))
    r))

(defn- scratch
  "A clone of a bare remote whose trunk is TRUNK, with an extra pushed branch
   \"staging\". The clone sits on TRUNK. => {:dir :remote}"
  [trunk]
  (let [base (str (fs/create-temp-dir {:prefix "bb-depsolve-branch"}))
        remote (str (fs/path base "remote.git"))
        seed (str (fs/path base "seed"))
        dir (str (fs/path base "clone"))]
    (git! base "init" "--bare" (str "--initial-branch=" trunk) remote)
    (fs/create-dirs seed)
    (git! seed "init" (str "--initial-branch=" trunk))
    (git! seed "config" "user.email" "test@example.com")
    (git! seed "config" "user.name" "test")
    (git! seed "config" "commit.gpgsign" "false")
    (spit (str (fs/path seed "f.txt")) "x\n")
    (git! seed "add" "f.txt")
    (git! seed "commit" "-m" "init")
    (git! seed "remote" "add" "origin" remote)
    (git! seed "push" "origin" trunk)
    (git! seed "push" "origin" (str trunk ":staging"))
    (git! base "clone" remote dir)
    {:dir dir :remote remote}))

(deftest resolve-default-prefers-the-forge-test
  (is (= {:default "master" :source :forge}
         (branch/resolve-default {:gh-default "master" :origin-head "origin/main"})))
  (is (= {:default "main" :source :origin-head}
         (branch/resolve-default {:origin-head "origin/main"
                                  :remote-branches ["main" "master"]})))
  (is (= {:default "master" :source :fallback}
         (branch/resolve-default {:remote-branches ["master" "staging"]})))
  (testing "both or neither trunk name: no guess"
    (is (= {:default nil :source :none}
           (branch/resolve-default {:remote-branches ["main" "master"]})))
    (is (= {:default nil :source :none} (branch/resolve-default {})))))

(deftest detect-test
  (testing "normal release path"
    (is (= {:status :on-default :current "main" :default "main"
            :source :origin-head :pr-flow? false}
           (branch/detect {:current "main" :origin-head "origin/main"}))))
  (testing "off the default"
    (let [v (branch/detect {:current "staging" :origin-head "origin/main"})]
      (is (= :off-default (:status v)))
      (is (true? (:pr-flow? v)))))
  (testing "detached HEAD and unknown default never trigger the PR flow"
    (is (= :detached (:status (branch/detect {:origin-head "origin/main"}))))
    (is (= :unknown (:status (branch/detect {:current "main"}))))
    (is (false? (:pr-flow? (branch/detect {:current "main"}))))))

(deftest git-facts-reads-a-clone-test
  (let [{:keys [dir]} (scratch "main")
        facts (branch/git-facts dir)]
    (is (= "main" (:current facts)))
    (is (= "origin/main" (:origin-head facts)))
    (is (= #{"main" "staging"} (set (:remote-branches facts))))))

(deftest detect-in-mixed-trunks-test
  (testing "a main repo on its trunk skips the PR flow"
    (let [{:keys [dir]} (scratch "main")]
      (is (= {:status :on-default :current "main" :default "main"
              :source :origin-head :pr-flow? false}
             (branch/detect-in dir)))))
  (testing "a master repo is resolved as master, not main"
    (let [{:keys [dir]} (scratch "master")]
      (is (= "master" (:default (branch/detect-in dir))))
      (is (false? (:pr-flow? (branch/detect-in dir))))))
  (testing "a release cut from staging triggers the PR flow"
    (let [{:keys [dir]} (scratch "main")]
      (git! dir "checkout" "staging")
      (is (= {:status :off-default :current "staging" :default "main"
              :source :origin-head :pr-flow? true}
             (branch/detect-in dir)))))
  (testing "the forge's answer is injected and wins"
    (let [{:keys [dir]} (scratch "main")]
      (is (= {:status :off-default :current "main" :default "staging"
              :source :forge :pr-flow? true}
             (branch/detect-in dir {:gh-default "staging"})))))
  (testing "a clone without origin/HEAD falls back to the one trunk name present"
    (let [{:keys [dir]} (scratch "master")]
      (git! dir "remote" "set-head" "origin" "-d")
      (is (nil? (:origin-head (branch/git-facts dir))))
      (is (= {:status :on-default :current "master" :default "master"
              :source :fallback :pr-flow? false}
             (branch/detect-in dir)))))
  (testing "a detached HEAD is reported, never an error"
    (let [{:keys [dir]} (scratch "main")]
      (git! dir "checkout" "--detach")
      (is (= :detached (:status (branch/detect-in dir)))))))
