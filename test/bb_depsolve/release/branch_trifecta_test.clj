(ns bb-depsolve.release.branch-trifecta-test
  "Golden, property and mutation checks for non-default release branch
   detection: the PR flow fires exactly when the current branch is not the
   default the facts name."
  (:require [bb-depsolve.release.branch :as branch]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

(defn- verdict
  "Detect over the facts alone."
  [facts]
  (branch/detect facts))

(defn- sound?
  "The verdict agrees with the facts: forge beats origin/HEAD, the PR flow
   fires only off-default, and no default means no claim."
  [{:keys [current gh-default origin-head] :as facts}]
  (let [{:keys [status default source pr-flow?]} (verdict facts)]
    (and (= (= status :off-default) pr-flow?)
         (case status
           :detached (nil? current)
           :unknown (nil? default)
           :on-default (= current default)
           :off-default (not= current default))
         (cond
           gh-default (and (= :forge source) (= gh-default default))
           origin-head (= :origin-head source)
           :else true))))

(defn- always-pr
  "Broken: opens the PR flow whatever branch the release is on."
  [facts]
  (assoc (branch/detect facts) :pr-flow? true))

(defn- ignore-forge
  "Broken: trusts origin/HEAD over the forge's answer."
  [facts]
  (branch/detect (dissoc facts :gh-default)))

(def ^:private branch-names (gen/elements ["main" "master" "staging" "release"]))

(tri/deftrifecta branch-detect-trifecta
  verdict
  {:golden-path "test/golden/bb-depsolve/branch-detect.edn"
   :cases {:on-main {:current "main" :origin-head "origin/main"}
           :feature-branch {:current "staging" :origin-head "origin/main"}
           :forge-beats-head {:current "main" :gh-default "master" :origin-head "origin/main"}
           :fallback-single {:current "master" :remote-branches ["master" "staging"]}
           :fallback-ambiguous {:current "main" :remote-branches ["main" "master"]}
           :detached {:origin-head "origin/main"}}
   :gen (gen/let [current (gen/one-of [(gen/return nil) branch-names])
                  gh-default (gen/one-of [(gen/return nil) branch-names])
                  head (gen/one-of [(gen/return nil) branch-names])
                  remote-branches (gen/vector branch-names 0 3)]
          (cond-> {:current current :remote-branches remote-branches}
            gh-default (assoc :gh-default gh-default)
            head (assoc :origin-head (str "origin/" head))))
   :pred sound?
   :num-tests 100
   :mutations [["always-pr" always-pr]
               ["ignore-forge" ignore-forge]]})
