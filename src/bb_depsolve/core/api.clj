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

(def git git/git)
(def auth-headers auth/auth-headers)
(def git-commits-ahead git/git-commits-ahead)
(def git-has-remote? git/git-has-remote?)
(def auto-commit-workspace! git/auto-commit-workspace!)
(def default-skip-dirs discovery/default-skip-dirs)
(def default-depth discovery/default-depth)
(def skip-path? discovery/skip-path?)
(def find-workspace-projects discovery/find-workspace-projects)
(def find-dep-files discovery/find-dep-files)
(def shadow-deps-file? discovery/shadow-deps-file?)
(def extract-mvn-deps discovery/extract-mvn-deps)
(def apply-mvn-change! upgrade/apply-mvn-change!)
(def resolve-local-tags resolve/resolve-local-tags)
(def resolve-remote-tags resolve/resolve-remote-tags)
(def resolve-lib-tags resolve/resolve-lib-tags)
(def resolve-clojars-latest registries/resolve-clojars-latest)
(def resolve-maven-latest registries/resolve-maven-latest)
(def gitea-registry-url auth/gitea-registry-url)
(def resolve-gitea-latest registries/resolve-gitea-latest)
(def resolve-mvn-latest registries/resolve-mvn-latest)

(def resolve-mvn-by-registry registries/resolve-mvn-by-registry)

(def resolve-mvn-reads registries/resolve-mvn-reads)
(def discover-internal-libs sync/discover-internal-libs)
(def compute-sync-changes sync/compute-sync-changes)

(def compute-withheld sync/compute-withheld)
(def apply-sync-changes! sync/apply-sync-changes!)
(def apply-mvn-upgrades! upgrade/apply-mvn-upgrades!)
(def sync-cmd sync/sync-cmd)

(def parity-cmd sync/parity-cmd)
(def upgrade-cmd upgrade/upgrade-cmd)
(def report-cmd report/report-cmd)
(def lint-cmd lint/lint-cmd)
(def find-consumers bump/find-consumers)
(def warn-major-bump! bump/warn-major-bump!)
(def bump-cmd bump/bump-cmd)
(def fetch-pom-deps fetch/fetch-pom-deps)
(def fetch-git-dep-coords fetch/fetch-git-dep-coords)
(def resolve-dep-children resolve/resolve-dep-children)
(def tree-cmd tree/tree-cmd)
(def c ui/c)
(def gum-table ui/gum-table)
(def gum-filter ui/gum-filter)
(def pad-right ui/pad-right)
(def matrix->csv ui/matrix->csv)
(def format-local-dep-warning ui/format-local-dep-warning)
