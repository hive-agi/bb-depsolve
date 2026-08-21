(ns bb-depsolve.core.push
  "Pure decisions for a workspace-wide push: what to do with each project,
   and how to settle the conflicts a dependency sweep produces."
  (:require [clojure.set :as set]))

(def default-conflict-policy
  "Path -> how to settle it when a sweep's merge conflicts.
   :ours keeps this workspace's file, :theirs takes the remote's,
   :remove drops a file this workspace deleted. Any other path aborts."
  {"deps.edn"  :ours
   "bb.edn"    :ours
   "VERSION"   :theirs
   "build.clj" :remove})

(defn conflict-action
  "How to settle PATH under POLICY. :abort when the policy has no entry."
  ([path] (conflict-action default-conflict-policy path))
  ([policy path] (get policy path :abort)))

(defn conflict-plan
  "Settle every path in CONFLICTS under POLICY.
   Returns {:resolvable? bool :actions {path action} :unknown [path ...]}."
  ([conflicts] (conflict-plan default-conflict-policy conflicts))
  ([policy conflicts]
   (let [actions (into {} (map (juxt identity #(conflict-action policy %))) conflicts)
         unknown (->> actions (keep (fn [[p a]] (when (= :abort a) p))) sort vec)]
     {:resolvable? (empty? unknown)
      :actions actions
      :unknown unknown})))

(defn push-plan
  "Decide what to do with one project from its observed git state. Pure.

   state keys: :has-remote? :upstream :ahead :behind :dirty-files :incoming-files
   Returns {:action :push | :merge-then-push | :skip, :reason kw, ...}.

   A project that is behind is merged first, EXCEPT when the incoming commits
   touch a file the working tree has modified — that tree belongs to whoever
   dirtied it."
  [{:keys [has-remote? upstream ahead behind dirty-files incoming-files]}]
  (let [ahead (or ahead 0)
        behind (or behind 0)]
    (cond
      (not has-remote?) {:action :skip :reason :no-remote}
      (nil? upstream) {:action :skip :reason :no-upstream}
      (zero? ahead) {:action :skip :reason :nothing-to-push}
      (zero? behind) {:action :push}
      :else
      (let [clash (set/intersection (set dirty-files) (set incoming-files))]
        (if (seq clash)
          {:action :skip :reason :wip-collision :files (vec (sort clash))}
          {:action :merge-then-push})))))

(defn plan-summary
  "Counts per action over a seq of plans. Pure."
  [plans]
  (reduce (fn [acc {:keys [action]}] (update acc action (fnil inc 0))) {} plans))
