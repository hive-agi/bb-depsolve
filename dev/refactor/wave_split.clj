(ns refactor.wave-split
  "Split spec for bb-depsolve.wave.commands (509 LOC) — plan-20260724-depsolve-srp
   step 16.

   The audit the step called for found no dead code: bb-depsolve.release.exec
   interprets a cascade PLAN, whereas wave holds workspace-wide operations
   with no cascade equivalent. release-wave-cmd is the fixed-pipeline
   predecessor of the cascade, but it still drives phases the cascade does
   not (external upgrade, lint --fix) and is CLI-wired. So nothing is
   deleted; wave keeps its release orchestration and sheds the rest.")

(def owner
  '{bump-wave-cmd "bb-depsolve.wave.commands"
    push-all-cmd "bb-depsolve.wave.commands"
    release-wave-cmd "bb-depsolve.wave.commands"

    default-gitignore-entries :hygiene
    ensure-gitignore-lines! :hygiene
    gitignore-cmd :hygiene
    rename-branch-cmd :hygiene

    lock-cmd :lock
    deep-lint-cmd :deep-lint
    help-cmd :help})

(def spec
  {:base-ns "bb-depsolve.wave.commands"
   :source-file "src/bb_depsolve/wave.clj"
   :source-dir "src/bb_depsolve/wave"
   :keep-module "bb-depsolve.wave.commands"
   :owner owner
   :external {}
   :base-requires [["fs" "[babashka.fs :as fs]"]
                   ["pp" "[clojure.pprint :as pp]"]
                   ["str" "[clojure.string :as str]"]
                   ["ba" "[hive-dsl.bounded-atom :as ba]"]
                   ["v" "[bb-depsolve.version.api :as v]"]
                   ["ui" "[bb-depsolve.cli.ui :as ui]"]
                   ["bump" "[bb-depsolve.core.bump :as bump]"]
                   ["discovery" "[bb-depsolve.core.discovery :as discovery]"]
                   ["fetch" "[bb-depsolve.core.fetch :as fetch]"]
                   ["git" "[bb-depsolve.core.git :as git]"]
                   ["lint" "[bb-depsolve.core.lint :as lint]"]
                   ["resolve" "[bb-depsolve.core.resolve :as resolve]"]
                   ["sync" "[bb-depsolve.core.sync :as sync]"]
                   ["upgrade" "[bb-depsolve.core.upgrade :as upgrade]"]]
   :module-doc
   {"bb-depsolve.wave.commands" "Workspace-wide release orchestration: bump-wave, push-all, release-wave."
    :hygiene "Workspace repo hygiene: .gitignore entries and default-branch rename."
    :lock "The lock command: deps.lock.edn per project."
    :deep-lint "Deep lint: check the latest tagged release for :local/root pins."
    :help "CLI help text."}})
