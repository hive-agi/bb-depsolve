(ns bb-depsolve.core.verify
  "Post-push verification: did the refs we pushed land on the remote?

   A push that exits 0 is not proof. The remote can serve a different sha for
   the branch, or never receive a tag (hive-agent's v0.4.3 reached no gitea
   although `git push --tags` said nothing). This namespace keeps the question
   pure: parse what `git ls-remote` printed, compare it with what we expected,
   and decide what the outcome means under the strict-push policy. The only
   effects live behind IPushProbe, whose git adapter is
   bb-depsolve.core.git/git-push-probe."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]))

(defprotocol IPushProbe
  "The two read-only questions post-push verification asks. Neither writes."
  (expected-refs [probe dir]
    "What a successful push of DIR must have produced:
     {:remote name :refs [{:ref \"refs/heads/main\" :sha s :kind :branch}
                         {:ref \"refs/tags/v1\" :sha s :kind :tag} ...]},
     or nil when DIR names no remote branch to check (detached, untracked).")
  (remote-refs [probe dir remote]
    "What REMOTE serves for its branches and tags. => Result of {ref sha}; an
     err carrying :message when the remote cannot be asked (no route, auth,
     timeout)."))

(defn parse-ls-remote
  "Parse `git ls-remote` output into {ref sha}. Peeled entries
   (`refs/tags/v1^{}`) keep their own key, so an annotated tag's object sha and
   its commit sha stay distinct. Blank and malformed lines are ignored."
  [out]
  (into {}
        (keep (fn [line]
                (let [[sha ref] (str/split (str/trim line) #"\s+" 2)]
                  (when (and (not (str/blank? sha)) (not (str/blank? ref)))
                    [(str/trim ref) sha]))))
        (str/split-lines (or out ""))))

(defn compare-refs
  "Compare EXPECTED ([{:ref :sha :kind}]) with OBSERVED ({ref sha}). Pure.
   => {:status :verified | :mismatch, :problems [...]}. A problem is
   {:ref :kind :expected :problem :missing | :differs} and, when :differs,
   the :observed sha. Refs OBSERVED holds beyond the expected ones are ignored.
   An annotated tag is served as the tag object plus a peeled `^{}` entry for
   its commit; an expected sha may be either, so the peeled entry also counts."
  [expected observed]
  (let [problems (into []
                       (keep (fn [{:keys [ref sha kind]}]
                               (let [got    (get observed ref)
                                     peeled (get observed (str ref "^{}"))]
                                 (cond
                                   (nil? got)
                                   {:ref ref :kind kind :expected sha :problem :missing}

                                   (or (= got sha) (= peeled sha))
                                   nil

                                   :else
                                   {:ref ref :kind kind :expected sha
                                    :observed got :problem :differs}))))
                       expected)]
    {:status (if (seq problems) :mismatch :verified)
     :problems problems}))

(defn verify-push!
  "Ask PROBE what DIR's push should have produced and what the remote serves,
   and compare. => {:status :verified | :mismatch | :unavailable, :remote name,
   :problems [...], :checked n, :detail text}. :unavailable means the answer
   could not be obtained (nothing to check, or the remote did not answer); it
   never means the push failed."
  [probe dir]
  (if-let [{:keys [remote refs]} (expected-refs probe dir)]
    (let [observed (remote-refs probe dir remote)]
      (if (r/ok? observed)
        (assoc (compare-refs refs (:ok observed)) :remote remote :checked (count refs))
        {:status :unavailable
         :remote remote
         :problems []
         :checked 0
         :detail (str "could not read refs from " remote ": "
                      (or (:message observed) "no answer"))}))
    {:status :unavailable
     :remote nil
     :problems []
     :checked 0
     :detail "no tracked remote branch to verify (detached HEAD or no upstream)"}))

(defn disposition
  "What a verification means to the cascade under the strict-push policy. Pure.
   => {:level :ok | :warn | :fail, :abort? bool}.

   :verified is :ok. :mismatch is always :fail: the remote visibly lacks what
   we pushed. :unavailable is a :warn by default and a :fail under STRICT?:
   a push that cannot be verified is not accepted as done. STRICT? also makes
   every :fail abort the cascade (:abort? true) instead of reporting and
   carrying on."
  [{:keys [status]} strict?]
  (let [strict? (boolean strict?)]
    (case status
      :verified    {:level :ok :abort? false}
      :mismatch    {:level :fail :abort? strict?}
      :unavailable (if strict?
                     {:level :fail :abort? true}
                     {:level :warn :abort? false})
      {:level :fail :abort? strict?})))

(defn- short-sha [s] (when s (subs s 0 (min 10 (count s)))))

(defn- problem-line
  [remote {:keys [ref expected observed problem]}]
  (case problem
    :missing (str ref " is not on " remote " (expected " (short-sha expected) ")")
    :differs (str ref " is " (short-sha observed) " on " remote
                  ", expected " (short-sha expected))
    (str ref " unverified on " remote)))

(defn describe
  "Report lines for one verification, each actionable. Pure. A :verified push
   yields one confirming line naming how many refs were checked; the problems
   of a :mismatch name the ref, what the remote serves, and what to run."
  [{:keys [status remote problems detail checked]}]
  (case status
    :verified    [(str "verified on " remote " (" checked " ref" (when (not= 1 checked) "s") ")")]
    :unavailable [(str "push not verified — " detail)]
    :mismatch    (conj (mapv #(problem-line remote %) problems)
                       (str "push exited 0 but " remote " does not serve what was pushed; "
                            "check `git ls-remote " remote "` and push the missing refs by hand"))
    [(str "unknown verification status " (pr-str status))]))
