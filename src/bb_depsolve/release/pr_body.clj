(ns bb-depsolve.release.pr-body
  "Pure release PR notes (M4.3). Git, VERSION, config and checkpoint reads
   belong to the caller; this namespace only folds their values into Markdown."
  (:require [clojure.string :as str]))

(def ^:private groups
  [[:feat "Features"]
   [:fix "Fixes"]
   [:refactor "Refactoring"]
   [:test "Tests"]
   [:chore "Chores"]
   [:other "Other changes"]])

(def default-test-plan
  "Placeholder used without a non-blank :pr-template :test-plan string."
  "- [ ] Add release verification steps.")

(defn- commit-type
  [subject]
  (if-let [[_ type] (re-find #"(?i)^(feat|fix|refactor|test|chore)(?:\([^)]+\))?!?:\s+\S" subject)]
    (keyword (str/lower-case type))
    :other))

(defn group-commits
  "Group GIT-LOG (git log --format=%s or --oneline text) by conventional
   prefix. Preserve newest-first input order, deduplicate subjects across SHAs,
   and keep at most two per group. Non-conventional subjects go in :other.
   => [{:type :feat|:fix|:refactor|:test|:chore|:other :subjects [string ...]}]"
  [git-log]
  (let [by-type (->> (str/split-lines (or git-log ""))
                     (map str/trim)
                     (remove str/blank?)
                     (map #(str/replace % #"^[0-9a-fA-F]{7,40}\s+" ""))
                     distinct
                     (group-by commit-type))]
    (into []
          (keep (fn [[type _]]
                  (when-let [subjects (seq (get by-type type))]
                    {:type type :subjects (vec (take 2 subjects))})))
          groups)))

(defn ripple-updates
  "Applied internal pin changes from an M1.3 CHECKPOINT's :waves/:steps.
   Include pins synced before a release failed, but not failed syncs, unresolved
   targets or no-ops. Collapse identical updates across manifest paths and sort
   for deterministic notes. No disk reads."
  [checkpoint]
  (->> (:waves checkpoint)
       (mapcat :steps)
       (filter #(contains? #{:released :release-failed} (:status %)))
       (mapcat (fn [{:keys [project pin-updates]}]
                 (for [{:keys [from to] :as pin} pin-updates
                       :when (and (some? to) (not= from to))]
                   (assoc (select-keys pin [:dep :lib :from :to]) :project project))))
       distinct
       (sort-by (juxt :project :dep :lib :from :to))
       vec))

(defn- changes-section
  [git-log]
  (let [grouped (group-commits git-log)
        titles (into {} groups)]
    (if (seq grouped)
      (str/join "\n\n"
                (map (fn [{:keys [type subjects]}]
                       (str "### " (get titles type) "\n\n"
                            (str/join "\n" (map #(str "- " %) subjects))))
                     grouped))
      "No commit subjects recorded for this release.")))

(defn- ripple-section
  [checkpoint]
  (let [updates (ripple-updates checkpoint)]
    (if (seq updates)
      (str/join "\n"
                (map (fn [{:keys [project dep lib from to]}]
                       (str "- `" project "`: `" (or lib dep) "` `"
                            (or from "(unversioned)") "` -> `" to "`."))
                     updates))
      "No internal dependency pin updates recorded.")))

(defn render
  "Render a release PR body from plain facts.
   OPTS: :tag :version :previous-version :base :head :git-log :checkpoint
         :pr-template {:test-plan Markdown-string} (from .bb-depsolve.edn).
   Missing facts get honest placeholders; no git, file or process effects."
  [{:keys [tag version previous-version base head git-log checkpoint pr-template]}]
  (let [test-plan (:test-plan pr-template)]
    (str "## Summary\n\n"
         "Merges `" head "` into `" base "`"
         (if-let [ships (or tag version)]
           (str " and ships `" ships "`.")
           ".")
         (when version
           (str "\n\nVERSION: "
                (when previous-version (str "`" previous-version "` -> "))
                "`" version "`."))
         "\n\n## Changes\n\n" (changes-section git-log)
         "\n\n## Sync ripple\n\n" (ripple-section checkpoint)
         "\n\n## Test plan\n\n"
         (if (and (string? test-plan) (not (str/blank? test-plan)))
           (str/trim test-plan)
           default-test-plan)
         "\n")))
