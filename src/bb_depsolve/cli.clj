(ns bb-depsolve.cli
  "CLI entry point for bb-depsolve.
   Dispatches to core commands via babashka.cli."
  (:require [babashka.cli :as cli]
            [bb-depsolve.audit :as audit]
            [bb-depsolve.core :as core]))

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
  [{:cmds ["sync"]    :fn (wrap-help core/sync-cmd    "sync"    "Sync internal git deps to latest tags")
    :doc "Sync internal git deps to latest tags"}
   {:cmds ["upgrade"] :fn (wrap-help core/upgrade-cmd  "upgrade" "Upgrade all deps to latest versions")
    :doc "Upgrade all deps to latest versions"}
   {:cmds ["report"]  :fn (wrap-help core/report-cmd   "report"  "Show dependency matrix")
    :doc "Show dependency matrix"}
   {:cmds ["lint"]    :fn (wrap-help core/lint-cmd     "lint"    "Detect dep anti-patterns (:local/root, etc.)")
    :doc "Detect dep anti-patterns (:local/root, etc.)"}
   {:cmds ["bump"]    :fn (wrap-help core/bump-cmd    "bump"    "Bump VERSION, tag, push, optionally sync downstream")
    :doc "Bump VERSION, tag, push, optionally sync downstream"}
   {:cmds ["tree"]    :fn (wrap-help core/tree-cmd    "tree"    "Show transitive dependency tree with conflict detection")
    :doc "Show transitive dependency tree with conflict detection"}
   {:cmds ["audit"]   :fn (wrap-help audit/audit-cmd  "audit"   "Scan dependencies for known CVEs (via OSV.dev)")
    :doc "Scan dependencies for known CVEs (via OSV.dev)"}
   {:cmds ["bump-wave"]    :fn (wrap-help core/bump-wave-cmd    "bump-wave"    "Bump all projects with commits ahead of their tag")
    :doc "Bump all projects with commits ahead of their tag"}
   {:cmds ["push-all"]     :fn (wrap-help core/push-all-cmd     "push-all"     "Push all workspace projects to remotes")
    :doc "Push all workspace projects to remotes"}
   {:cmds ["release-wave"] :fn (wrap-help core/release-wave-cmd "release-wave" "Full release: upgrade → lint → sync → bump → push")
    :doc "Full release: upgrade → lint → sync → bump → push"}
   {:cmds []          :fn (fn [_] (core/help-cmd dispatch-table))}])

(defn -main [& args]
  (cli/dispatch dispatch-table args
                {:coerce {:apply :boolean
                          :fix :boolean
                          :pre-release :boolean
                          :conflicts-only :boolean
                          :commit :boolean
                          :help :boolean
                          :major :boolean
                          :minor :boolean
                          :stable :boolean
                          :sync :boolean
                          :depth :long
                          :tree-depth :long}}))
