(ns bb-depsolve.core.cross-name-trifecta-test
  "Golden + property + mutation coverage for cross-name-tracking.

   The preflight lint reports a local branch that tracks an upstream branch of
   a different name (local `main` -> `gitea/master`), which hid a silently
   failing push in hive-agent. It reports only; nothing is renamed."
  (:require [bb-depsolve.core.push :as push]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private same-name
  {:branch "main" :merge-ref "refs/heads/main" :remote "origin"})

(def ^:private cross-name
  {:branch "main" :merge-ref "refs/heads/master" :remote "gitea"})

(def ^:private slashed-same
  {:branch "feature/x" :merge-ref "refs/heads/feature/x" :remote "origin"})

(def ^:private slashed-cross
  {:branch "tw8/a" :merge-ref "refs/heads/tw8/b" :remote "origin"})

(def ^:private detached
  {:branch nil :merge-ref nil :remote nil})

(def ^:private untracked
  {:branch "main" :merge-ref nil :remote nil})

;; =============================================================================
;; Generator: same-name and cross-name tracking, slashes included
;; =============================================================================

(def ^:private gen-name
  (gen/let [parts (gen/vector (gen/not-empty gen/string-alphanumeric) 1 3)]
    (str/join "/" parts)))

(def ^:private gen-tracking
  (gen/let [branch gen-name
            other gen-name
            same? gen/boolean
            remote (gen/elements ["origin" "gitea" "github"])]
    {:branch branch
     :merge-ref (str "refs/heads/" (if same? branch other))
     :remote remote}))

;; =============================================================================
;; Predicate: the oracle, stated independently of the implementation
;; =============================================================================

(defn- reports-exactly-cross-names?
  "Predicate over the subject's OUTPUT: silence, or a report whose two names
   really differ and whose upstream name has refs/heads/ stripped."
  [out]
  (or (nil? out)
      (and (map? out)
           (string? (:branch out))
           (string? (:upstream-branch out))
           (not= (:branch out) (:upstream-branch out))
           (not (str/starts-with? (:upstream-branch out) "refs/heads/")))))

;; =============================================================================
;; Mutations: each must be caught
;; =============================================================================

(defn- never-reports
  "Broken: the lint is silent."
  [_tracking]
  nil)

(defn- compares-full-ref
  "Broken: compares the branch to the unstripped refs/heads/... ref, so every
   tracked branch reads as cross-name."
  [{:keys [branch merge-ref remote]}]
  (when (and branch merge-ref (not= branch merge-ref))
    {:branch branch :upstream-branch merge-ref :remote remote}))

(defn- last-segment-only
  "Broken: keeps only the last path segment, so tw8/a vs tw8/b is hidden
   and feature/x vs refs/heads/feature/x is misread."
  [{:keys [branch merge-ref remote]}]
  (when (and branch merge-ref)
    (let [up (last (str/split merge-ref #"/"))]
      (when (not= branch up)
        {:branch branch :upstream-branch up :remote remote}))))

;; =============================================================================
;; Trifecta
;; =============================================================================

(tri/deftrifecta cross-name-tracking-trifecta
  push/cross-name-tracking
  {:golden-path "test/golden/bb-depsolve/cross-name-tracking.edn"
   :cases       {:same-name     same-name
                 :cross-name    cross-name
                 :slashed-same  slashed-same
                 :slashed-cross slashed-cross
                 :detached      detached
                 :untracked     untracked}
   :gen         gen-tracking
   :pred        reports-exactly-cross-names?
   :num-tests   200
   :mutations   [["never-reports"     never-reports]
                 ["compares-full-ref" compares-full-ref]
                 ["last-segment-only" last-segment-only]]})

(deftest the-hive-agent-case-is-reported-test
  (is (= {:branch "main" :upstream-branch "master" :remote "gitea"}
         (push/cross-name-tracking cross-name))))

(deftest detached-and-untracked-are-silent-test
  (is (nil? (push/cross-name-tracking detached)))
  (is (nil? (push/cross-name-tracking untracked))))

(deftest reports-exactly-the-cross-named-inputs-test
  (doseq [t (gen/sample gen-tracking 200)
          :let [upstream (subs (:merge-ref t) (count "refs/heads/"))
                out (push/cross-name-tracking t)]]
    (if (= (:branch t) upstream)
      (is (nil? out) (str "same name must be silent: " t))
      (is (= {:branch (:branch t) :upstream-branch upstream :remote (:remote t)} out)
          (str "cross name must be reported: " t)))))
