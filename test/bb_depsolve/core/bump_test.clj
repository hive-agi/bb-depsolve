(ns bb-depsolve.core.bump-test
  "Tests for the bump command: flag mapping, the bump plan, and the --apply gate."
  (:require [babashka.fs :as fs]
            [babashka.process :as proc]
            [bb-depsolve.core.bump :as bump]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- project!
  "A throwaway project directory holding VERSION."
  [version]
  (let [dir (str (fs/create-temp-dir {:prefix "bb-depsolve-bump"}))]
    (spit (str (fs/path dir "VERSION")) (str version "\n"))
    dir))

(defn- version-of [dir]
  (str/trim (slurp (str (fs/path dir "VERSION")))))

(defn- recording-sh
  "proc/sh stand-in: records the argv and reports success."
  [calls]
  (fn [args & _] (swap! calls conj (vec args)) {:exit 0 :out "" :err ""}))

(defn- git-verbs [calls]
  (mapv #(nth % 3 nil) @calls))

;; =============================================================================
;; Unit — bump-level
;; =============================================================================

(deftest bump-level-maps-each-flag-to-its-own-segment-test
  (is (= :major (bump/bump-level {:major true})) "--major bumps the major segment")
  (is (= :minor (bump/bump-level {:minor true})) "--minor bumps the minor segment")
  (is (= :patch (bump/bump-level {})) "patch is the default")
  (is (= :major (bump/bump-level {:stable true}))
      "--stable is a major promotion — on a 0.x version that is 1.0.0"))

(deftest bump-level-takes-the-strongest-flag-test
  (is (= :major (bump/bump-level {:major true :minor true})))
  (is (= :major (bump/bump-level {:stable true :minor true}))))

;; =============================================================================
;; Unit — plan-bump
;; =============================================================================

(deftest plan-bump-describes-the-resulting-version-test
  (is (= {:current "0.4.2" :level :patch :new-version "0.4.3" :new-tag "v0.4.3"}
         (bump/plan-bump "0.4.2" {})))
  (is (= {:current "0.4.2" :level :minor :new-version "0.5.0" :new-tag "v0.5.0"}
         (bump/plan-bump "0.4.2" {:minor true})))
  (is (= {:current "0.4.2" :level :major :new-version "1.0.0" :new-tag "v1.0.0"}
         (bump/plan-bump "0.4.2" {:major true})))
  (is (= "1.0.0" (:new-version (bump/plan-bump "0.4.2" {:stable true})))
      "--stable promotes a pre-1.0 project to 1.0.0"))

(deftest plan-bump-is-nil-for-an-unparseable-version-test
  (is (nil? (bump/plan-bump "not-a-version" {})))
  (is (nil? (bump/plan-bump "" {}))))

;; =============================================================================
;; Unit — bump-cmd, the --apply gate
;; =============================================================================

(deftest bump-cmd-without-apply-writes-nothing-test
  (let [dir (project! "0.4.2")
        calls (atom [])
        out (with-out-str
              (with-redefs [proc/sh (recording-sh calls)]
                (bump/bump-cmd {:opts {:root dir}})))]
    (is (= "0.4.2" (version-of dir)) "the VERSION file is left alone")
    (is (= [] @calls) "no git command runs — nothing is committed, tagged or pushed")
    (is (str/includes? out "0.4.3") "the planned version is still reported")
    (is (str/includes? out "--apply") "and the dry run says how to execute it")))

(deftest bump-cmd-with-apply-writes-tags-and-pushes-test
  (let [dir (project! "0.4.2")
        calls (atom [])]
    (with-out-str
      (with-redefs [proc/sh (recording-sh calls)]
        (bump/bump-cmd {:opts {:root dir :apply true}})))
    (is (= "0.4.3" (version-of dir)))
    (is (= ["add" "commit" "tag" "push" "push"] (git-verbs calls)))
    (is (some #(= "v0.4.3" (last %)) @calls) "the new tag reaches git")))

(deftest bump-cmd-applies-the-requested-level-test
  (testing "--minor yields a minor bump, not a patch"
    (let [dir (project! "0.4.2")]
      (with-out-str
        (with-redefs [proc/sh (recording-sh (atom []))]
          (bump/bump-cmd {:opts {:root dir :apply true :minor true}})))
      (is (= "0.5.0" (version-of dir)))))
  (testing "--major yields a major bump, not a minor"
    (let [dir (project! "0.4.2")]
      (with-out-str
        (with-redefs [proc/sh (recording-sh (atom []))]
          (bump/bump-cmd {:opts {:root dir :apply true :major true}})))
      (is (= "1.0.0" (version-of dir))))))

(deftest bump-cmd-updates-nested-version-files-test
  (let [dir (project! "0.4.2")
        nested (str (fs/path dir "sub" "VERSION"))]
    (fs/create-dirs (fs/path dir "sub"))
    (spit nested "0.4.2\n")
    (with-out-str
      (with-redefs [proc/sh (recording-sh (atom []))]
        (bump/bump-cmd {:opts {:root dir :apply true}})))
    (is (= "0.4.3" (str/trim (slurp nested))))))

;; =============================================================================
;; Protected branch — the tag must not outrun a rejected branch push
;; =============================================================================

(defn- stub-git
  "A git port that records argv and fails the verbs in FAILING (a set of
   argv vectors) with a GH006-shaped stderr."
  [calls failing]
  (fn [& args]
    (swap! calls conj (vec args))
    (if (contains? failing (vec args))
      {:exit 1 :out "" :err "remote: error: GH006: Protected branch update failed for refs/heads/main."}
      {:exit 0 :out "" :err ""})))

(deftest publish-release-skips-the-tag-push-when-the-branch-push-is-rejected-test
  (let [calls (atom [])
        res   (bump/publish-release! (stub-git calls #{["push"]}) "v0.4.3")]
    (is (= :bump/branch-push-rejected (get-in res [:error :type])))
    (is (str/includes? (get-in res [:error :stderr]) "GH006"))
    (is (not-any? #(= ["push" "--tags"] %) @calls)
        "the tag never reaches the remote, so it cannot land orphaned ahead of main")))

(deftest publish-release-pushes-the-tag-after-the-branch-lands-test
  (let [calls (atom [])
        res   (bump/publish-release! (stub-git calls #{}) "v0.4.3")]
    (is (= {:tag "v0.4.3" :pushed #{:branch :tag}} (:ok res)))
    (is (= [["commit" "-m" "release: v0.4.3"] ["tag" "v0.4.3"] ["push"] ["push" "--tags"]]
           @calls))))

(deftest publish-release-stops-on-a-failed-commit-test
  (let [calls (atom [])
        res   (bump/publish-release! (stub-git calls #{["commit" "-m" "release: v0.4.3"]}) "v0.4.3")]
    (is (= :bump/git-failed (get-in res [:error :type])))
    (is (= 1 (count @calls)) "nothing is tagged or pushed after a failed commit")))

(defn- sh! [dir & args]
  (let [r (proc/sh (into ["git" "-C" (str dir)] args))]
    (when-not (zero? (:exit r)) (throw (ex-info "fixture git failed" {:args args :r r})))
    r))

(deftest bump-against-a-protected-remote-leaves-no-orphan-tag-test
  (testing "a temp remote that refuses the branch update but accepts tags — the
            GH006 asymmetry. A non-bare remote with main checked out has exactly
            that shape (receive.denyCurrentBranch=refuse), no hook needed."
    (let [root   (str (fs/create-temp-dir {:prefix "bb-depsolve-protected"}))
          remote (str (fs/path root "remote"))
          dir    (str (fs/path root "work"))]
      (sh! root "init" "-q" "-b" "main" remote)
      (sh! remote "config" "user.email" "t@t") (sh! remote "config" "user.name" "t")
      (spit (str (fs/path remote "VERSION")) "0.4.2\n")
      (sh! remote "add" "VERSION") (sh! remote "commit" "-q" "-m" "init")
      (sh! root "clone" "-q" remote dir)
      (sh! dir "config" "user.email" "t@t") (sh! dir "config" "user.name" "t")
      (let [exit (atom nil)
            out  (with-out-str
                   (bump/bump-cmd {:opts {:root dir :apply true}
                                   :exit! #(reset! exit %)}))]
        (is (= "" (str/trim (:out (proc/sh ["git" "-C" remote "tag" "--list"]))))
            "the remote holds no tag: v0.4.3 was never pushed")
        (is (= "v0.4.3" (str/trim (:out (proc/sh ["git" "-C" dir "tag" "--list"]))))
            "the tag is kept locally for the PR-merge recovery")
        (is (= 1 @exit) "bump reports failure")
        (is (str/includes? out "rejected"))
        (is (str/includes? out "v0.4.3"))))))
