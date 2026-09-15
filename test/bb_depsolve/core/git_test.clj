(ns bb-depsolve.core.git-test
  "Tests for bb-depsolve.core.git against throwaway local repos with a bare
   remote. Every effect lands in a temp dir the test creates."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.git :as git]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- git! [dir & args]
  (let [r (apply git/git dir args)]
    (when-not (zero? (:exit r))
      (throw (ex-info (str "git " (str/join " " args) " failed") r)))
    r))

(defn- configure! [dir]
  (git! dir "config" "user.email" "test@example.com")
  (git! dir "config" "user.name" "test")
  (git! dir "config" "commit.gpgsign" "false"))

(defn- commit-file! [dir fname content msg]
  (spit (str (fs/path dir fname)) content)
  (git! dir "add" fname)
  (git! dir "commit" "-m" msg))

(defn- scratch
  "A repo `top` with one committed deps.edn, pushed to a bare remote with
   upstream tracking. => {:base :dir :remote}"
  []
  (let [base (str (fs/create-temp-dir {:prefix "bb-depsolve-core-git"}))
        remote (str (fs/path base "remote.git"))
        dir (str (fs/path base "top"))]
    (fs/create-dirs dir)
    (git! base "init" "--bare" "--initial-branch=main" remote)
    (git! dir "init" "--initial-branch=main")
    (configure! dir)
    (commit-file! dir "deps.edn" "{:deps {}}\n" "init")
    (git! dir "remote" "add" "origin" remote)
    (git! dir "push" "-u" "origin" "main")
    {:base base :dir dir :remote remote}))

(defn- push-from-peer!
  "Clone REMOTE beside the scratch repo, commit FNAME there and push it, so the
   scratch repo has an incoming commit once it fetches."
  [{:keys [base remote]} fname content]
  (let [peer (str (fs/path base "peer"))]
    (git! base "clone" remote peer)
    (configure! peer)
    (commit-file! peer fname content (str "peer: " fname))
    (git! peer "push" "origin" "main")
    peer))

(defn- head-files [dir]
  (set (remove str/blank?
               (str/split-lines
                (:out (git! dir "show" "--name-only" "--format=" "HEAD"))))))

;; =============================================================================
;; git / git-has-remote? / git-upstream
;; =============================================================================

(deftest git-runs-in-the-given-directory-test
  (let [{:keys [dir]} (scratch)
        r (git/git dir "rev-parse" "--show-toplevel")]
    (is (zero? (:exit r)))
    (is (= (str (fs/canonicalize dir)) (str/trim (:out r))))))

(deftest git-has-remote?-test
  (let [dir (str (fs/create-temp-dir {:prefix "bb-depsolve-core-git"}))]
    (git! dir "init" "--initial-branch=main")
    (is (false? (git/git-has-remote? dir)) "a fresh repo has no remote")
    (git! dir "remote" "add" "origin" "https://example.invalid/r.git")
    (is (true? (git/git-has-remote? dir))))
  (testing "a directory that is not a repo has no remote"
    (is (false? (git/git-has-remote? (str (fs/create-temp-dir)))))))

(deftest git-upstream-test
  (let [{:keys [dir]} (scratch)]
    (is (= "origin/main" (git/git-upstream dir)))
    (git! dir "checkout" "-b" "local-only")
    (is (nil? (git/git-upstream dir)) "a branch with no tracking ref has no upstream")))

;; =============================================================================
;; git-commits-ahead / git-ahead-behind
;; =============================================================================

(deftest git-commits-ahead-test
  (let [{:keys [dir]} (scratch)]
    (git! dir "tag" "v0.1.0")
    (is (= 0 (git/git-commits-ahead dir "v0.1.0")))
    (commit-file! dir "a.txt" "a" "one")
    (commit-file! dir "b.txt" "b" "two")
    (is (= 2 (git/git-commits-ahead dir "v0.1.0")))
    (is (= 0 (git/git-commits-ahead dir "v9.9.9"))
        "an unknown tag counts as 0, not as an error")))

(deftest git-ahead-behind-test
  (let [{:keys [dir] :as s} (scratch)]
    (is (= {:ahead 0 :behind 0} (git/git-ahead-behind dir "origin/main")))
    (commit-file! dir "local.txt" "l" "local")
    (push-from-peer! s "remote.txt" "r")
    (git! dir "fetch" "origin")
    (is (= {:ahead 1 :behind 1} (git/git-ahead-behind dir "origin/main")))
    (is (= {:ahead 0 :behind 0} (git/git-ahead-behind dir "origin/no-such-branch"))
        "an unresolvable upstream degrades to zero on both sides")))

;; =============================================================================
;; git-dirty-files / git-incoming-files / git-conflicted-files
;; =============================================================================

(deftest git-dirty-files-test
  (let [{:keys [dir]} (scratch)]
    (is (= [] (git/git-dirty-files dir)))
    (spit (str (fs/path dir "deps.edn")) "{:deps {a/b {:mvn/version \"1\"}}}\n")
    (spit (str (fs/path dir "untracked.txt")) "u")
    (is (= ["deps.edn"] (git/git-dirty-files dir))
        "untracked files cannot conflict with an incoming commit")
    (spit (str (fs/path dir "staged.txt")) "s")
    (git! dir "add" "staged.txt")
    (is (= #{"deps.edn" "staged.txt"} (set (git/git-dirty-files dir)))
        "a staged addition is dirty")))

(deftest git-incoming-files-test
  (let [{:keys [dir] :as s} (scratch)]
    (is (= [] (git/git-incoming-files dir "origin/main")))
    (push-from-peer! s "incoming.edn" "{}")
    (git! dir "fetch" "origin")
    (is (= ["incoming.edn"] (git/git-incoming-files dir "origin/main")))
    (is (= [] (git/git-incoming-files dir "origin/no-such-branch")))))

(deftest git-conflicted-files-test
  (let [{:keys [dir] :as s} (scratch)]
    (is (= [] (git/git-conflicted-files dir)))
    (push-from-peer! s "deps.edn" "{:deps {peer/lib {:mvn/version \"2\"}}}\n")
    (commit-file! dir "deps.edn" "{:deps {local/lib {:mvn/version \"1\"}}}\n" "local")
    (git! dir "fetch" "origin")
    (is (not (zero? (:exit (git/git dir "merge" "origin/main"))))
        "precondition: the merge conflicts")
    (is (= ["deps.edn"] (git/git-conflicted-files dir))
        "a path appears once even though both probes report it")))

;; =============================================================================
;; auto-commit-workspace!
;; =============================================================================

(deftest auto-commit-workspace!-commits-only-dep-files-test
  (let [{:keys [base dir]} (scratch)]
    (commit-file! dir "src.clj" "(ns src)" "src")
    (spit (str (fs/path dir "deps.edn")) "{:deps {a/b {:mvn/version \"2\"}}}\n")
    (spit (str (fs/path dir "src.clj")) "(ns src) ;; edited")
    (let [before (:out (git! dir "rev-parse" "HEAD"))
          out (with-out-str
                (git/auto-commit-workspace! base [{:project "top"} {:project "top"}]
                                            "chore: sync deps"))]
      (is (str/includes? out "Committed: top"))
      (is (= 1 (count (re-seq #"Committed" out)))
          "a project listed by several dep files is committed once")
      (is (not= before (:out (git! dir "rev-parse" "HEAD"))))
      (is (= #{"deps.edn"} (head-files dir)))
      (is (= "chore: sync deps"
             (str/trim (:out (git! dir "log" "-1" "--format=%s")))))
      (is (= ["src.clj"] (git/git-dirty-files dir))
          "a non-dep change is left for its author"))))

(deftest auto-commit-workspace!-is-silent-without-changes-test
  (let [{:keys [base dir]} (scratch)
        before (:out (git! dir "rev-parse" "HEAD"))
        out (with-out-str
              (git/auto-commit-workspace! base [{:project "top"}] "chore: sync deps"))]
    (is (= "" out))
    (is (= before (:out (git! dir "rev-parse" "HEAD"))))))
