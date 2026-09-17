(ns bb-depsolve.core.push-test
  "The push decision table and the sweep's conflict policy — both pure."
  (:require [bb-depsolve.core.push :as push]
            [clojure.test :refer [deftest is testing]]))

(def ^:private base
  {:has-remote? true :upstream "origin/main" :ahead 1 :behind 0
   :dirty-files [] :incoming-files []})

;; =============================================================================
;; push-plan
;; =============================================================================

(deftest nothing-to-push-is-a-skip-test
  (is (= {:action :skip :reason :nothing-to-push}
         (push/push-plan (assoc base :ahead 0)))))

(deftest a-remoteless-project-is-a-skip-test
  (is (= {:action :skip :reason :no-remote}
         (push/push-plan (assoc base :has-remote? false)))))

(deftest a-branch-with-no-upstream-is-a-skip-test
  (is (= {:action :skip :reason :no-upstream}
         (push/push-plan (assoc base :upstream nil)))))

(deftest an-up-to-date-upstream-pushes-directly-test
  (is (= {:action :push} (push/push-plan base))))

(deftest a-moved-upstream-is-merged-first-test
  (testing "the CI release commit case: behind, and nothing local is dirty"
    (is (= {:action :merge-then-push}
           (push/push-plan (assoc base :behind 1 :incoming-files ["VERSION"]))))))

(deftest a-dirty-tree-is-merged-when-the-incoming-commits-miss-it-test
  (is (= {:action :merge-then-push}
         (push/push-plan (assoc base :behind 1
                                :dirty-files ["src/a.clj"]
                                :incoming-files ["VERSION" "deps.edn"])))))

(deftest an-incoming-commit-touching-someone-elses-wip-is-a-skip-test
  (let [plan (push/push-plan (assoc base :behind 1
                                    :dirty-files ["VERSION" "src/a.clj"]
                                    :incoming-files ["VERSION"]))]
    (is (= :skip (:action plan)))
    (is (= :wip-collision (:reason plan)))
    (is (= ["VERSION"] (:files plan))
        "the report names the file, so the skip is actionable")))

(deftest untracked-work-does-not-block-a-merge-test
  (testing "git-dirty-files excludes untracked paths, so they never intersect"
    (is (= {:action :merge-then-push}
           (push/push-plan (assoc base :behind 2
                                  :dirty-files []
                                  :incoming-files ["deps.edn"]))))))

;; =============================================================================
;; conflict policy
;; =============================================================================

(deftest the-sweeps-own-files-resolve-to-ours-test
  (is (= :ours (push/conflict-action "deps.edn")))
  (is (= :ours (push/conflict-action "bb.edn"))
      "our side of a dep sweep carries the newer pins"))

(deftest version-belongs-to-ci-test
  (is (= :theirs (push/conflict-action "VERSION"))))

(deftest a-file-the-migration-deleted-is-dropped-test
  (is (= :remove (push/conflict-action "build.clj"))))

(deftest an-unknown-path-aborts-test
  (is (= :abort (push/conflict-action "src/hive/core.clj"))))

(deftest a-plan-is-resolvable-only-when-every-path-is-known-test
  (let [ok (push/conflict-plan ["deps.edn" "build.clj" "VERSION"])]
    (is (:resolvable? ok))
    (is (= {"deps.edn" :ours "build.clj" :remove "VERSION" :theirs} (:actions ok)))
    (is (empty? (:unknown ok))))
  (let [nope (push/conflict-plan ["deps.edn" "src/hive/core.clj"])]
    (is (not (:resolvable? nope)))
    (is (= ["src/hive/core.clj"] (:unknown nope))
        "one unknown path is enough to leave the merge alone")))

(deftest an-empty-conflict-set-is-resolvable-test
  (is (:resolvable? (push/conflict-plan []))))

;; =============================================================================
;; plan-summary
;; =============================================================================

(deftest summary-counts-by-action-test
  (is (= {:push 2 :skip 1}
         (push/plan-summary [{:action :push} {:action :skip :reason :no-remote} {:action :push}]))))

(deftest a-landed-push-whose-tags-were-refused-is-still-pushed-test
  (let [out (push/push-outcome {:branch {:exit 0 :out "" :err ""}
                                :tags   {:exit 1 :out ""
                                         :err "To origin\n ! [rejected]        v0.4.28 -> v0.4.28 (already exists)\nerror: failed to push some refs"}})]
    (is (= :pushed (:status out)) "the ref update landed; the verdict is the branch push")
    (is (= :failed (:tags out)))
    (is (= "! [rejected]        v0.4.28 -> v0.4.28 (already exists)" (:tags-detail out))
        "git's first informative stderr line, so the row is actionable")
    (is (nil? (:detail out)))))

(deftest a-non-fast-forward-is-rejected-not-failed-test
  (let [out (push/push-outcome {:branch {:exit 1 :out ""
                                         :err "To origin\n ! [rejected]        HEAD -> main (non-fast-forward)\nhint: use 'git pull' before pushing again."}
                                :tags   {:exit 0 :out "" :err ""}})]
    (is (= :rejected (:status out)) "the upstream moved; --sync merges it")
    (is (= :pushed (:tags out)))
    (is (re-find #"non-fast-forward" (:detail out)))))

(deftest any-other-branch-failure-is-failed-with-its-reason-test
  (is (= {:status :failed :tags :skipped :detail "fatal: unable to access 'x': Could not resolve host"}
         (push/push-outcome {:branch {:exit 128 :out "" :err "fatal: unable to access 'x': Could not resolve host\n"}
                             :tags nil})))
  (is (= {:status :failed :tags :skipped :detail "no output"}
         (push/push-outcome {:branch {:exit 1 :out "" :err ""} :tags nil}))
      "a silent failure still says so")
  (is (= {:status :pushed :tags :pushed}
         (push/push-outcome {:branch {:exit 0} :tags {:exit 0}}))))
