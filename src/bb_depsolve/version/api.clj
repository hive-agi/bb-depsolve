(ns bb-depsolve.version.api
  "Facade over the version submodules. Re-exports only."
  (:require [bb-depsolve.schema.api]
            [bb-depsolve.version.lib :as lib]
            [bb-depsolve.version.maven :as maven]
            [bb-depsolve.version.parse :as parse]
            [bb-depsolve.version.rewrite :as rewrite]
            [bb-depsolve.version.semver :as semver]
            [bb-depsolve.version.tree :as tree]
            [bb-depsolve.version.vuln :as vuln]
            [bb-depsolve.version.repos :as repos]))

(defn parse-semver [& args] (apply semver/parse-semver args))
(defn pre-release? [& args] (apply semver/pre-release? args))
(defn stable? [& args] (apply semver/stable? args))
(defn parse-version-segments [& args] (apply semver/parse-version-segments args))
(defn version-newer? [& args] (apply semver/version-newer? args))
(defn version-compare [& args] (apply semver/version-compare args))
(defn latest-tag [& args] (apply semver/latest-tag args))
(defn find-git-deps [& args] (apply parse/find-git-deps args))

(defn parse-local-tag-output [& args] (apply parse/parse-local-tag-output args))
(defn parse-ls-remote-tags [& args] (apply parse/parse-ls-remote-tags args))
(defn find-git-sha-only-deps [& args] (apply parse/find-git-sha-only-deps args))
(defn dep-coords-by-scope [& args] (apply parse/dep-coords-by-scope args))
(defn runtime-libs [& args] (apply parse/runtime-libs args))
(defn find-mvn-deps [& args] (apply parse/find-mvn-deps args))
(defn find-shadow-deps [& args] (apply parse/find-shadow-deps args))
(defn update-shadow-dep [& args] (apply rewrite/update-shadow-dep args))
(defn find-local-deps [& args] (apply parse/find-local-deps args))

(defn declared-libs [& args] (apply parse/declared-libs args))
(defn replace-local-with-git [& args] (apply rewrite/replace-local-with-git args))
(defn replace-local-with-mvn [& args] (apply rewrite/replace-local-with-mvn args))
(defn update-git-dep [& args] (apply rewrite/update-git-dep args))
(defn update-mvn-dep [& args] (apply rewrite/update-mvn-dep args))
(defn sha-matches? [& args] (apply rewrite/sha-matches? args))
(defn pick-sha [& args] (apply rewrite/pick-sha args))
(defn tag->mvn-version [& args] (apply semver/tag->mvn-version args))
(defn sync-changes-in-content [& args] (apply rewrite/sync-changes-in-content args))

(defn downgrade-change? [& args] (apply rewrite/downgrade-change? args))

(defn declared-repos [& args] (apply repos/declared-repos args))

(defn project-resolved [& args] (apply repos/project-resolved args))

(defn withheld-pins [& args] (apply repos/withheld args))
(defn bump-patch [& args] (apply semver/bump-patch args))
(defn bump-minor [& args] (apply semver/bump-minor args))
(defn bump-major [& args] (apply semver/bump-major args))
(defn semver->tag [& args] (apply semver/semver->tag args))
(defn semver->version [& args] (apply semver/semver->version args))
(defn parse-github-lib [& args] (apply lib/parse-github-lib args))
(defn parse-forge-lib [& args] (apply lib/parse-forge-lib args))
(defn forge-clone-url [& args] (apply lib/forge-clone-url args))
(defn forge-raw-url [& args] (apply lib/forge-raw-url args))
(defn major-bump? [& args] (apply semver/major-bump? args))
(defn infer-sibling-dir [& args] (apply lib/infer-sibling-dir args))
(defn lib-matches-org? [& args] (apply lib/lib-matches-org? args))
(defn lib-artifact-id [& args] (apply lib/lib-artifact-id args))
(defn canonical-lib [& args] (apply lib/canonical-lib args))
(defn maven-property? [& args] (apply maven/maven-property? args))
(defn unresolved-property? [& args] (apply maven/unresolved-property? args))
(defn filter-resolved-coords [& args] (apply maven/filter-resolved-coords args))
(defn group-id->path [& args] (apply lib/group-id->path args))
(defn maven-metadata-url [& args] (apply maven/maven-metadata-url args))
(defn parse-maven-metadata-versions [& args] (apply maven/parse-maven-metadata-versions args))
(defn latest-published-version [& args] (apply maven/latest-published-version args))
(defn pom-urls [& args] (apply maven/pom-urls args))
(defn parse-pom-deps-raw [& args] (apply maven/parse-pom-deps-raw args))
(defn parse-pom-deps [& args] (apply maven/parse-pom-deps args))

(defn parse-pom-properties [& args] (apply maven/parse-pom-properties args))
(defn pom-parent-coord [& args] (apply maven/pom-parent-coord args))
(defn substitute-properties [& args] (apply maven/substitute-properties args))
(defn parse-pom-deps-resolved [& args] (apply maven/parse-pom-deps-resolved args))
(defn deps-edn->dep-coords [& args] (apply parse/deps-edn->dep-coords args))
(defn build-dep-tree [& args] (apply tree/build-dep-tree args))
(defn find-conflicts [& args] (apply tree/find-conflicts args))
(defn collect-occurrences [& args] (apply tree/collect-occurrences args))
(defn resolve-versions [& args] (apply tree/resolve-versions args))
(defn format-dep-tree [& args] (apply tree/format-dep-tree args))
(defn lib->maven-coord [& args] (apply lib/lib->maven-coord args))
(defn parse-osv-vuln [& args] (apply vuln/parse-osv-vuln args))
(defn severity-rank [& args] (apply vuln/severity-rank args))
(defn sort-vulns-by-severity [& args] (apply vuln/sort-vulns-by-severity args))

(defn recommended-fix [& args] (apply vuln/recommended-fix args))