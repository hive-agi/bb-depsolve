(ns refactor.core-split
  "Split spec for bb-depsolve.core.api (1048 LOC) — plan-20260724-depsolve-srp
   steps 9 through 14.")

(def owner
  "Var -> owning submodule."
  '{git :git git-changed-files :git git-commits-ahead :git git-has-remote? :git
    auto-commit-project! :git auto-commit-workspace! :git

    auth-headers :auth github-url :auth gitea-registry-url :auth

    http-gate :fetch fetch-pom-xml :fetch warn-unresolved-coord :fetch
    fetch-pom-deps :fetch fetch-git-deps-edn :fetch fetch-git-dep-coords :fetch

    default-skip-dirs :discovery default-depth :discovery skip-path? :discovery
    find-workspace-projects :discovery find-dep-files :discovery
    shadow-deps-file? :discovery extract-mvn-deps :discovery

    resolve-local-tags :resolve resolve-remote-tags :resolve
    resolve-lib-tags :resolve resolve-clojars-latest :resolve
    resolve-maven-latest :resolve resolve-gitea-latest :resolve
    resolve-mvn-latest :resolve resolve-dep-children :resolve

    discover-internal-libs :sync compute-sync-changes :sync
    apply-sync-changes! :sync sync-cmd :sync

    apply-mvn-change! :upgrade apply-mvn-upgrades! :upgrade upgrade-cmd :upgrade

    ensure-gitignore-entry! :lint generate-local-deps-edn :lint lint-cmd :lint

    find-consumers :bump warn-major-bump! :bump bump-cmd :bump

    report-cmd :report
    tree-cmd :tree})

(def ui-vars
  "The bb-depsolve.cli.ui re-export block core.clj carried. Submodules reach these
   through the ui alias instead."
  '[c gum-table gum-filter pad-right visible-len matrix->csv tty? colors
    format-local-dep-warning])

(def spec
  {:base-ns "bb-depsolve.core.api"
   :source-file "src/bb_depsolve/core.clj"
   :source-dir "src/bb_depsolve/core"
   :facade-doc "Facade over the core submodules. Re-exports only."
   :owner owner

   ;; Vars that keep their name but resolve elsewhere.
   :external (zipmap ui-vars (repeat "ui"))

   ;; The ui re-export block is regenerated in the facade, so drop the originals.
   :drop-forms ui-vars

   ;; visible-len, tty? and colors had no callers; dropping them also removes
   ;; core.clj's reach into the private bb-depsolve.cli.ui/colors.
   :extra-exports '[[c "bb-depsolve.cli.ui"]
                    [gum-table "bb-depsolve.cli.ui"]
                    [gum-filter "bb-depsolve.cli.ui"]
                    [pad-right "bb-depsolve.cli.ui"]
                    [matrix->csv "bb-depsolve.cli.ui"]
                    [format-local-dep-warning "bb-depsolve.cli.ui"]]

   ;; Reached from a sibling module, so they cannot stay private.
   :promote-public '#{github-url fetch-git-deps-edn}

   :base-requires [["fs" "[babashka.fs :as fs]"]
                   ["proc" "[babashka.process :as proc]"]
                   ["http" "[babashka.http-client :as http]"]
                   ["gum" "[bblgum.core :as gum]"]
                   ["json" "[cheshire.core :as json]"]
                   ["str" "[clojure.string :as str]"]
                   ["ba" "[hive-dsl.bounded-atom :as ba]"]
                   ["gate" "[hive-dsl.gate :as gate]"]
                   ["r" "[hive-dsl.result :as r]"]
                   ["v" "[bb-depsolve.version.api :as v]"]
                   ["ui" "[bb-depsolve.cli.ui :as ui]"]
                   ["sch" "[bb-depsolve.schema.api :as sch]"]]

   :module-doc
   {:git "Git process helpers: status, commits-ahead, workspace auto-commit."
    :auth "Forge credentials and registry endpoints."
    :fetch "Gated HTTP retrieval of POM and deps.edn artifacts."
    :discovery "Workspace scanning: dep-file discovery and project layout."
    :resolve "Registry resolution: tags and latest published versions."
    :sync "The sync command: discover, compute and apply internal pin changes."
    :upgrade "The upgrade command: mvn dependency upgrades."
    :lint "The lint command: :local/root anti-pattern detection and fixes."
    :bump "The bump command: VERSION bump, tag, push."
    :report "The report command: dependency matrix."
    :tree "The tree command: transitive dependency tree."}})
