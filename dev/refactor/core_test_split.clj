(ns refactor.core-test-split
  "Split spec for test/bb_depsolve/core_test.clj — plan-20260724-depsolve-srp
   step 15.

   The file is named for bb-depsolve.core.api but its own docstring says
   \"version.clj layer\", and all 38 deftests exercise version functions. It
   therefore lands beside the version submodule suites, not the core tree.")

(def owner
  '{parse-semver-test :semver-test
    version-newer?-test :semver-test
    pre-release?-test :semver-test
    latest-tag-test :semver-test
    bump-patch-test :semver-test
    bump-minor-test :semver-test
    bump-major-test :semver-test
    semver->tag-test :semver-test
    semver->version-test :semver-test
    tag->mvn-version-test :semver-test

    find-git-deps-test :parse-test
    find-mvn-deps-test :parse-test
    find-shadow-deps-test :parse-test
    find-local-deps-test :parse-test
    deps-edn->dep-coords-test :parse-test

    update-shadow-dep-test :rewrite-test
    update-git-dep-test :rewrite-test
    update-mvn-dep-test :rewrite-test
    sha-matches?-test :rewrite-test
    replace-local-with-git-test :rewrite-test
    replace-local-with-mvn-test :rewrite-test
    sync-changes-in-content-test :rewrite-test

    parse-github-lib-test :lib-test
    lib-matches-org?-test :lib-test
    lib-artifact-id-test :lib-test
    group-id->path-test :lib-test

    maven-property?-test :maven-test
    unresolved-property?-test :maven-test
    filter-resolved-coords-test :maven-test
    parse-pom-deps-raw-keeps-unresolved-test :maven-test
    parse-pom-deps-filters-maven-properties-test :maven-test
    pom-urls-test :maven-test
    parse-pom-deps-test :maven-test
    unresolved-property-integration-test :maven-test

    build-dep-tree-test :tree-test
    find-conflicts-test :tree-test
    format-dep-tree-test :tree-test
    resolve-versions-test :tree-test})

(def spec
  {:base-ns "bb-depsolve.version.api"
   :source-file "test/bb_depsolve/core_test.clj"
   :source-dir "test/bb_depsolve/version"
   :facade? false
   :owner owner
   :external {}
   :always-requires ["[clojure.test :refer [deftest is testing]]"
                     "[bb-depsolve.version.api :as v]"]
   :base-requires [["str" "[clojure.string :as str]"]]
   :module-doc
   {:semver-test "Unit tests for semver arithmetic."
    :parse-test "Unit tests for dependency extraction."
    :rewrite-test "Unit tests for dependency rewriting."
    :lib-test "Unit tests for lib identity and coordinate naming."
    :maven-test "Unit tests for Maven metadata and POM handling."
    :tree-test "Unit tests for dependency trees."}})
