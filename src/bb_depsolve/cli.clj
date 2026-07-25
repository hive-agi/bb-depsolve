(ns bb-depsolve.cli
  "CLI entry point for bb-depsolve.
   Dispatches to core commands via babashka.cli."
  (:require [babashka.cli :as cli]
            [bb-depsolve.audit :as audit]
            [bb-depsolve.core.bump :as bump]
            [bb-depsolve.core.lint :as lint]
            [bb-depsolve.core.report :as report]
            [bb-depsolve.core.sync :as sync]
            [bb-depsolve.core.upgrade :as upgrade]
            [bb-depsolve.release :as release]
            [bb-depsolve.wave :as wave]
            [bb-depsolve.core.tree :as tree]
            [bb-depsolve.wave.deep-lint :as deep-lint]
            [bb-depsolve.wave.lock :as lock]
            [bb-depsolve.wave.hygiene :as hygiene]
            [bb-depsolve.wave.help :as help]
            [bb-depsolve.release.inspect :as inspect]))

(defn- wrap-help
  "Wrap a command fn so --help prints subcommand usage instead of executing."
  [cmd-fn cmd-name doc]
  (fn [{:keys [opts] :as m}]
    (if (:help opts)
      (do (println (str "Usage: bb-depsolve " cmd-name " [options]"))
          (println)
          (println (str "  " doc))
          (println)
          (println "Run bb-depsolve --help for all options."))
      (cmd-fn m))))

(def dispatch-table
  [{:cmds ["sync"], :fn (wrap-help sync/sync-cmd "sync" "Sync internal Git coords to tags and Maven coords to published registry versions"), :doc "Sync internal Git and Maven coords to their authoritative sources"}
   {:cmds ["upgrade"] :fn (wrap-help upgrade/upgrade-cmd  "upgrade" "Upgrade all deps to latest versions")
    :doc "Upgrade all deps to latest versions"}
   {:cmds ["report"]  :fn (wrap-help report/report-cmd   "report"  "Show dependency matrix")
    :doc "Show dependency matrix"}
   {:cmds ["lint"]    :fn (wrap-help lint/lint-cmd     "lint"    "Detect dep anti-patterns (:local/root, etc.)")
    :doc "Detect dep anti-patterns (:local/root, etc.)"}
   {:cmds ["deep-lint"] :fn (wrap-help deep-lint/deep-lint-cmd "deep-lint" "Lint latest tagged releases for :local/root")
    :doc "Lint latest tagged releases for :local/root anti-patterns"}
   {:cmds ["bump"]    :fn (wrap-help bump/bump-cmd    "bump"    "Bump VERSION, tag, push, optionally sync downstream")
    :doc "Bump VERSION, tag, push, optionally sync downstream"}
   {:cmds ["tree"]    :fn (wrap-help tree/tree-cmd    "tree"    "Show transitive dependency tree with conflict detection")
    :doc "Show transitive dependency tree with conflict detection"}
   {:cmds ["lock"]    :fn (wrap-help lock/lock-cmd    "lock"    "Generate deps.lock.edn per project")
    :doc "Generate deterministic deps.lock.edn per project"}
   {:cmds ["audit"]   :fn (wrap-help audit/audit-cmd  "audit"   "Scan dependencies for known CVEs (via OSV.dev)")
    :doc "Scan dependencies for known CVEs (via OSV.dev)"}
   {:cmds ["graph"]   :fn (wrap-help inspect/graph-cmd "graph"  "Show the internal dependency DAG in release order")
    :doc "Show the internal dependency DAG in release order"}
   {:cmds ["impact"]  :fn (wrap-help inspect/impact-cmd "impact" "Show what releasing one project forces downstream")
    :doc "Show what releasing one project forces downstream"}
   {:cmds ["cascade"] :fn (wrap-help release/cascade-cmd "cascade" "Plan a transitive release cascade across the workspace")
    :doc "Plan a transitive release cascade across the workspace"}
   {:cmds ["reconcile"] :fn (wrap-help release/reconcile-cmd "reconcile" "Rewrite VERSION files that lag their released versions")
    :doc "Rewrite VERSION files that lag their released versions"}
   {:cmds ["bump-wave"]    :fn (wrap-help wave/bump-wave-cmd    "bump-wave"    "Bump all projects with commits ahead of their tag")
    :doc "Bump all projects with commits ahead of their tag"}
   {:cmds ["push-all"]     :fn (wrap-help wave/push-all-cmd     "push-all"     "Push all workspace projects to remotes")
    :doc "Push all workspace projects to remotes"}
   {:cmds ["release-wave"] :fn (wrap-help wave/release-wave-cmd "release-wave" "Full release: upgrade → lint → sync → bump → push")
    :doc "Full release: upgrade → lint → sync → bump → push"}
   {:cmds ["gitignore"]    :fn (wrap-help hygiene/gitignore-cmd    "gitignore"    "Auto-add .gitignore entries to workspace projects")
    :doc "Auto-add .gitignore entries (target/, .cpcache/, etc.)"}
   {:cmds ["rename-branch"] :fn (wrap-help hygiene/rename-branch-cmd "rename-branch" "Rename master->main across workspace")
    :doc "Rename branch (default master->main) across workspace"}
   {:cmds []          :fn (fn [_] (help/help-cmd dispatch-table))}])

(defn -main [& args]
  (cli/dispatch dispatch-table args
                {:coerce {:apply :boolean
                          :fix :boolean
                          :pre-release :boolean
                          :conflicts-only :boolean
                          :resolved :boolean
                          :commit :boolean
                          :help :boolean
                          :major :boolean
                          :minor :boolean
                          :stable :boolean
                          :sync :boolean
                          :force :boolean
                          :skip-remote :boolean
                          :no-wait :boolean
                          :depth :long
                          :tree-depth :long
                          :await-timeout :long
                          :from :string
                          :to :string
                          :lib :string
                          :format :string
                          :extra :string}}))
