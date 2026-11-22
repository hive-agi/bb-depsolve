(ns bb-depsolve.core.api
  "Facade over the core submodules. Re-exports only."
  (:require [bb-depsolve.core.auth :as auth]
            [bb-depsolve.core.bump :as bump]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.fetch :as fetch]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.core.lint :as lint]
            [bb-depsolve.core.report :as report]
            [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.core.sync :as sync]
            [bb-depsolve.core.tree :as tree]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.core.upgrade :as upgrade]
            [bb-depsolve.core.resolve.registries :as registries]))

(defn git [& args] (apply git/git args))
(defn auth-headers [& args] (apply auth/auth-headers args))
(defn git-commits-ahead [& args] (apply git/git-commits-ahead args))
(defn git-has-remote? [& args] (apply git/git-has-remote? args))
(defn auto-commit-workspace! [& args] (apply git/auto-commit-workspace! args))
(def default-skip-dirs discovery/default-skip-dirs)
(def default-depth discovery/default-depth)
(defn skip-path? [& args] (apply discovery/skip-path? args))
(defn find-workspace-projects [& args] (apply discovery/find-workspace-projects args))
(defn find-dep-files [& args] (apply discovery/find-dep-files args))
(defn shadow-deps-file? [& args] (apply discovery/shadow-deps-file? args))
(defn extract-mvn-deps [& args] (apply discovery/extract-mvn-deps args))
(defn apply-mvn-change! [& args] (apply upgrade/apply-mvn-change! args))
(defn resolve-local-tags [& args] (apply resolve/resolve-local-tags args))
(defn resolve-remote-tags [& args] (apply resolve/resolve-remote-tags args))
(defn resolve-lib-tags [& args] (apply resolve/resolve-lib-tags args))
(defn resolve-clojars-latest [& args] (apply registries/resolve-clojars-latest args))
(defn resolve-maven-latest [& args] (apply registries/resolve-maven-latest args))
(defn gitea-registry-url [& args] (apply auth/gitea-registry-url args))
(defn resolve-gitea-latest [& args] (apply registries/resolve-gitea-latest args))
(defn resolve-mvn-latest [& args] (apply registries/resolve-mvn-latest args))

(defn resolve-mvn-by-registry [& args] (apply registries/resolve-mvn-by-registry args))

(defn resolve-mvn-reads [& args] (apply registries/resolve-mvn-reads args))
(defn discover-internal-libs [& args] (apply sync/discover-internal-libs args))
(defn compute-sync-changes [& args] (apply sync/compute-sync-changes args))

(defn compute-withheld [& args] (apply sync/compute-withheld args))
(defn apply-sync-changes! [& args] (apply sync/apply-sync-changes! args))
(defn apply-mvn-upgrades! [& args] (apply upgrade/apply-mvn-upgrades! args))
(defn sync-cmd [& args] (apply sync/sync-cmd args))

(defn parity-cmd [& args] (apply sync/parity-cmd args))
(defn upgrade-cmd [& args] (apply upgrade/upgrade-cmd args))
(defn report-cmd [& args] (apply report/report-cmd args))
(defn lint-cmd [& args] (apply lint/lint-cmd args))
(defn find-consumers [& args] (apply bump/find-consumers args))
(defn warn-major-bump! [& args] (apply bump/warn-major-bump! args))
(defn bump-cmd [& args] (apply bump/bump-cmd args))
(defn fetch-pom-deps [& args] (apply fetch/fetch-pom-deps args))
(defn fetch-git-dep-coords [& args] (apply fetch/fetch-git-dep-coords args))
(defn resolve-dep-children [& args] (apply resolve/resolve-dep-children args))
(defn tree-cmd [& args] (apply tree/tree-cmd args))
(defn c [& args] (apply ui/c args))
(defn gum-table [& args] (apply ui/gum-table args))
(defn gum-filter [& args] (apply ui/gum-filter args))
(defn pad-right [& args] (apply ui/pad-right args))
(defn matrix->csv [& args] (apply ui/matrix->csv args))
(defn format-local-dep-warning [& args] (apply ui/format-local-dep-warning args))
