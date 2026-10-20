(ns bb-depsolve.core.verify-test
  "Post-push verification: the pure comparison and disposition under trifectas,
   the orchestration against a stub IPushProbe (no git, no network), the git
   adapter against throwaway repos with a bare remote, and push-all's
   --strict-push cascade policy end to end through the probe port."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.core.verify :as verify]
            [bb-depsolve.wave.commands :as wave]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-dsl.result :as r]
            [hive-test.trifecta :as tri]))

(def ^:private sha-a (apply str (repeat 40 \a)))
(def ^:private sha-b (apply str (repeat 40 \b)))
(def ^:private sha-c (apply str (repeat 40 \c)))

;; =============================================================================
;; Pure: compare-refs
;; =============================================================================

(def ^:private branch {:ref "refs/heads/main" :sha sha-a :kind :branch})
(def ^:private tag {:ref "refs/tags/v1.0.0" :sha sha-b :kind :tag})

(defn- compare-case
  "One trifecta subject input: the expected refs and what the remote serves."
  [{:keys [expected observed]}]
  (verify/compare-refs expected observed))

(defn- compared? [{:keys [status problems]}]
  (and (contains? #{:verified :mismatch} status)
       (vector? problems)
       (= (= :verified status) (empty? problems))))

(tri/deftrifecta compare-refs-trifecta
  compare-case
  {:golden-path "test/golden/bb-depsolve/verify-compare-refs.edn"
   :cases {:all-landed {:expected [branch tag]
                        :observed {"refs/heads/main" sha-a "refs/tags/v1.0.0" sha-b}}
           :tag-missing {:expected [branch tag]
                         :observed {"refs/heads/main" sha-a}}
           :branch-elsewhere {:expected [branch]
                              :observed {"refs/heads/main" sha-c}}
           :both-wrong {:expected [branch tag]
                        :observed {"refs/tags/v1.0.0" sha-c}}
           :extra-remote-refs-ignored {:expected [branch]
                                       :observed {"refs/heads/main" sha-a
                                                  "refs/heads/other" sha-c}}
           :annotated-tag-peeled {:expected [{:ref "refs/tags/v1.0.0" :sha sha-c :kind :tag}]
                                  :observed {"refs/tags/v1.0.0" sha-b
                                             "refs/tags/v1.0.0^{}" sha-c}}
           :nothing-expected {:expected [] :observed {}}}
   :gen (gen/fmap (fn [[got-branch got-tag]]
                    {:expected [branch tag]
                     :observed (cond-> {}
                                 got-branch (assoc "refs/heads/main" got-branch)
                                 got-tag (assoc "refs/tags/v1.0.0" got-tag))})
                  (gen/tuple (gen/elements [nil sha-a sha-c])
                             (gen/elements [nil sha-b sha-c])))
   :pred compared?
   :num-tests 60
   :mutations [["always-verified" (fn [_] {:status :verified :problems []})]
               ["missing-is-fine" (fn [{:keys [expected observed]}]
                                    (verify/compare-refs
                                     (filter #(contains? observed (:ref %)) expected)
                                     observed))]]})

(deftest compare-refs-problems-test
  (testing "a missing ref and a ref at another sha are told apart"
    (let [got (verify/compare-refs [branch tag] {"refs/heads/main" sha-c})]
      (is (= :mismatch (:status got)))
      (is (= [{:ref "refs/heads/main" :kind :branch :expected sha-a
               :observed sha-c :problem :differs}
              {:ref "refs/tags/v1.0.0" :kind :tag :expected sha-b :problem :missing}]
             (:problems got))))))

;; =============================================================================
;; Pure: parse-ls-remote
;; =============================================================================

(deftest parse-ls-remote-test
  (is (= {"refs/heads/main" sha-a
          "refs/tags/v1" sha-b
          "refs/tags/v1^{}" sha-c}
         (verify/parse-ls-remote
          (str sha-a "\trefs/heads/main\n"
               sha-b "\trefs/tags/v1\n"
               sha-c "\trefs/tags/v1^{}\n"))))
  (testing "blank and malformed lines are ignored"
    (is (= {"refs/heads/main" sha-a}
           (verify/parse-ls-remote (str "\n" sha-a "\trefs/heads/main\nlonely\n  \n"))))
    (is (= {} (verify/parse-ls-remote nil)))
    (is (= {} (verify/parse-ls-remote "")))))

;; =============================================================================
;; Pure: disposition
;; =============================================================================

(defn- dispose [{:keys [status strict?]}]
  (verify/disposition {:status status} strict?))

(tri/deftrifecta disposition-trifecta
  dispose
  {:golden-path "test/golden/bb-depsolve/verify-disposition.edn"
   :cases {:verified {:status :verified :strict? false}
           :verified-strict {:status :verified :strict? true}
           :mismatch {:status :mismatch :strict? false}
           :mismatch-strict {:status :mismatch :strict? true}
           :unavailable {:status :unavailable :strict? false}
           :unavailable-strict {:status :unavailable :strict? true}
           :unknown-status {:status :weird :strict? false}}
   :gen (gen/fmap (fn [[s strict?]] {:status s :strict? strict?})
                  (gen/tuple (gen/elements [:verified :mismatch :unavailable])
                             gen/boolean))
   :pred (fn [{:keys [level abort?]}]
           (and (contains? #{:ok :warn :fail} level) (boolean? abort?)))
   :num-tests 40
   :mutations [["strict-ignored" (fn [{:keys [status]}] (verify/disposition {:status status} false))]
               ["never-aborts" (fn [_] {:level :fail :abort? false})]]})

(deftest strict-policy-test
  (testing "a mismatch is a failure with or without --strict-push; only strict aborts"
    (is (= {:level :fail :abort? false} (verify/disposition {:status :mismatch} false)))
    (is (= {:level :fail :abort? true} (verify/disposition {:status :mismatch} true))))
  (testing "an unverifiable push is a warning, and a failure under --strict-push"
    (is (= {:level :warn :abort? false} (verify/disposition {:status :unavailable} nil)))
    (is (= {:level :fail :abort? true} (verify/disposition {:status :unavailable} true))))
  (testing "a verified push never aborts"
    (is (= {:level :ok :abort? false} (verify/disposition {:status :verified} true)))))

;; =============================================================================
;; Orchestration over a stub port
;; =============================================================================

(defrecord StubProbe [expected observed]
  verify/IPushProbe
  (expected-refs [_ _] expected)
  (remote-refs [_ _ _] observed))

(def ^:private expectation {:remote "origin" :refs [branch tag]})

(deftest verify-push-verified-test
  (let [probe (->StubProbe expectation
                           (r/ok {"refs/heads/main" sha-a "refs/tags/v1.0.0" sha-b}))
        got   (verify/verify-push! probe "/x")]
    (is (= :verified (:status got)))
    (is (= "origin" (:remote got)))
    (is (= ["verified on origin (2 refs)"] (verify/describe got)))))

(deftest verify-push-mismatch-reports-an-actionable-message-test
  (let [probe (->StubProbe expectation (r/ok {"refs/heads/main" sha-a}))
        got   (verify/verify-push! probe "/x")
        lines (verify/describe got)]
    (is (= :mismatch (:status got)))
    (is (= "refs/tags/v1.0.0 is not on origin (expected bbbbbbbbbb)" (first lines)))
    (is (re-find #"git ls-remote origin" (last lines))
        "the last line says what to run")))

(deftest verify-push-differing-sha-names-both-test
  (let [probe (->StubProbe expectation
                           (r/ok {"refs/heads/main" sha-c "refs/tags/v1.0.0" sha-b}))
        lines (verify/describe (verify/verify-push! probe "/x"))]
    (is (= "refs/heads/main is cccccccccc on origin, expected aaaaaaaaaa" (first lines)))))

(deftest verify-push-unreachable-remote-is-unavailable-not-mismatch-test
  (let [probe (->StubProbe expectation
                           (r/err :git/ls-remote-failed {:message "fatal: could not resolve host"}))
        got   (verify/verify-push! probe "/x")]
    (is (= :unavailable (:status got)))
    (is (empty? (:problems got)))
    (is (= ["push not verified — could not read refs from origin: fatal: could not resolve host"]
           (verify/describe got)))))

(deftest verify-push-nothing-to-check-is-unavailable-test
  (let [got (verify/verify-push! (->StubProbe nil (r/ok {})) "/x")]
    (is (= :unavailable (:status got)))
    (is (nil? (:remote got)))))

;; =============================================================================
;; The git adapter against real repos with a bare remote
;; =============================================================================

(defn- git! [dir & args]
  (let [res (apply git/git dir args)]
    (when-not (zero? (:exit res))
      (throw (ex-info (str "git " (str/join " " args) " failed") res)))
    res))

(defn- configure! [dir]
  (git! dir "config" "user.email" "test@example.com")
  (git! dir "config" "user.name" "test")
  (git! dir "config" "commit.gpgsign" "false")
  (git! dir "config" "tag.gpgsign" "false"))

(defn- scratch-repo!
  "A repo NAME under BASE with one commit, a bare remote and upstream
   tracking. => repo dir"
  [base name]
  (let [remote (str (fs/path base (str name ".git")))
        dir    (str (fs/path base name))]
    (fs/create-dirs dir)
    (git! base "init" "--bare" "--initial-branch=main" remote)
    (git! dir "init" "--initial-branch=main")
    (configure! dir)
    (spit (str (fs/path dir "f.txt")) "one\n")
    (git! dir "add" "f.txt")
    (git! dir "commit" "-m" "one")
    (git! dir "remote" "add" "origin" remote)
    (git! dir "push" "-u" "origin" "main")
    dir))

(defn- temp-base [] (str (fs/create-temp-dir {:prefix "bb-depsolve-verify"})))

(defn- commit-more! [dir content]
  (spit (str (fs/path dir "f.txt")) content)
  (git! dir "commit" "-am" content))

(deftest git-probe-verifies-a-real-push-test
  (let [dir (scratch-repo! (temp-base) "top")
        probe (git/git-push-probe)]
    (commit-more! dir "two\n")
    (git! dir "tag" "v1.0.0")
    (git! dir "tag" "-a" "-m" "annotated" "v1.0.1")
    (git/git-push! dir)
    (let [got (verify/verify-push! probe dir)]
      (is (= :verified (:status got)) (pr-str got))
      (is (= 3 (:checked got)) "the branch plus the two tags at HEAD"))))

(deftest git-probe-catches-an-unpushed-tag-test
  (testing "the v0.4.3 failure: the branch landed, the tag never did"
    (let [dir (scratch-repo! (temp-base) "top")]
      (commit-more! dir "two\n")
      (git! dir "push")
      (git! dir "tag" "v2.0.0")
      (let [got (verify/verify-push! (git/git-push-probe) dir)]
        (is (= :mismatch (:status got)))
        (is (= [{:ref "refs/tags/v2.0.0" :kind :tag :problem :missing
                 :expected (str/trim (:out (git! dir "rev-parse" "refs/tags/v2.0.0")))}]
               (:problems got)))))))

(deftest git-probe-catches-an-unpushed-branch-test
  (let [dir (scratch-repo! (temp-base) "top")]
    (commit-more! dir "two\n")
    (let [got (verify/verify-push! (git/git-push-probe) dir)]
      (is (= :mismatch (:status got)))
      (is (= [:differs] (mapv :problem (:problems got))))
      (is (= "refs/heads/main" (:ref (first (:problems got))))))))

(deftest git-probe-unreachable-remote-is-unavailable-test
  (let [base (temp-base)
        dir  (scratch-repo! base "top")]
    (fs/delete-tree (str (fs/path base "top.git")))
    (let [got (verify/verify-push! (git/git-push-probe) dir)]
      (is (= :unavailable (:status got)))
      (is (str/starts-with? (first (verify/describe got))
                            "push not verified — could not read refs from origin:")))))

(deftest git-probe-detached-head-has-nothing-to-check-test
  (let [dir (scratch-repo! (temp-base) "top")]
    (git! dir "checkout" "--detach")
    (is (= :unavailable (:status (verify/verify-push! (git/git-push-probe) dir))))))

;; =============================================================================
;; push-all: --strict-push over the probe port
;; =============================================================================

(defn- workspace!
  "A workspace root holding repos named NAMES, each with an unpushed commit."
  [names]
  (let [base (temp-base)
        root (str (fs/path base "ws"))]
    (fs/create-dirs root)
    (doseq [n names]
      (let [dir (scratch-repo! root n)]
        (commit-more! dir (str n " two\n"))))
    root))

(defn- run-push-all
  "Run push-all with PROBE; => {:out text :exit code-or-nil :seen [dir-name ...]}."
  [root probe-fn extra-opts]
  (let [exit (atom nil)
        seen (atom [])
        probe (reify verify/IPushProbe
                (expected-refs [_ dir]
                  (swap! seen conj (str (fs/file-name dir)))
                  (:expected (probe-fn dir)))
                (remote-refs [_ dir _]
                  (:observed (probe-fn dir))))
        out (with-out-str
              (wave/push-all-cmd {:opts (merge {:root root} extra-opts)
                                  :probe probe
                                  :exit! #(reset! exit %)}))]
    {:out out :exit @exit :seen @seen}))

(defn- failing-probe [bad-name]
  (fn [dir]
    (if (= bad-name (str (fs/file-name dir)))
      {:expected expectation :observed (r/ok {})}
      {:expected expectation
       :observed (r/ok {"refs/heads/main" sha-a "refs/tags/v1.0.0" sha-b})})))

(deftest push-all-reports-a-mismatch-and-carries-on-test
  (let [root (workspace! ["a" "b" "c"])
        {:keys [out exit seen]} (run-push-all root (failing-probe "b") {})]
    (is (nil? exit) "no --strict-push: the exit port is never called")
    (is (= ["a" "b" "c"] seen) "every pushed project was verified")
    (is (re-find #"refs/heads/main is not on origin" out))
    (is (re-find #"Mismatched: 1" out))
    (is (re-find #"Pushed: 3" out))))

(deftest push-all-strict-stops-the-cascade-at-the-first-mismatch-test
  (let [root (workspace! ["a" "b" "c"])
        {:keys [out exit seen]} (run-push-all root (failing-probe "b") {:strict-push true})]
    (is (= 1 exit))
    (is (= ["a" "b"] seen) "c was never pushed or verified")
    (is (re-find #"stopping the cascade" out))
    (is (re-find #"Pushed: 2" out))))

(deftest push-all-strict-aborts-on-an-unverifiable-push-test
  (let [root (workspace! ["a" "b"])
        {:keys [exit out]} (run-push-all
                            root
                            (fn [_] {:expected expectation
                                     :observed (r/err :git/ls-remote-failed {:message "boom"})})
                            {:strict-push true})]
    (is (= 1 exit))
    (is (re-find #"push not verified" out))
    (is (re-find #"Unverified: 1" out))))

(deftest push-all-without-strict-warns-on-an-unverifiable-push-test
  (let [root (workspace! ["a" "b"])
        {:keys [exit out]} (run-push-all
                            root
                            (fn [_] {:expected expectation
                                     :observed (r/err :git/ls-remote-failed {:message "boom"})})
                            {})]
    (is (nil? exit))
    (is (re-find #"Unverified: 2" out))
    (is (re-find #"Pushed: 2" out))))

(deftest push-all-no-verify-push-skips-the-probe-test
  (let [root (workspace! ["a"])
        {:keys [exit seen out]} (run-push-all root (failing-probe "a")
                                              {:strict-push true :no-verify-push true})]
    (is (nil? exit))
    (is (empty? seen))
    (is (re-find #"Mismatched: 0" out))))

(deftest push-all-verified-push-prints-confirmation-test
  (let [root (workspace! ["a"])
        {:keys [exit out]} (run-push-all root (failing-probe "none") {:strict-push true})]
    (is (nil? exit))
    (is (re-find #"verified on origin \(2 refs\)" out))))
