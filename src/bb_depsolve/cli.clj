(ns bb-depsolve.cli
  "CLI entry point for bb-depsolve.
   Dispatches to core commands via babashka.cli."
  (:require [babashka.cli :as cli]
            [bb-depsolve.core :as core]))

(def dispatch-table
  [{:cmds ["sync"]    :fn core/sync-cmd    :doc "Sync internal git deps to latest tags"}
   {:cmds ["upgrade"] :fn core/upgrade-cmd  :doc "Upgrade all deps to latest versions"}
   {:cmds ["report"]  :fn core/report-cmd   :doc "Show dependency matrix"}
   {:cmds ["lint"]    :fn core/lint-cmd     :doc "Detect dep anti-patterns (:local/root, etc.)"}
   {:cmds []          :fn (fn [_] (core/help-cmd dispatch-table))}])

(defn -main [& args]
  (cli/dispatch dispatch-table args
                {:coerce {:apply :boolean
                          :fix :boolean
                          :pre-release :boolean
                          :depth :long}}))
