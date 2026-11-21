(ns bb-depsolve.graph.collect
  "Workspace collectors for the internal dependency graph.

   Collect layer: reads version.edn, VERSION and dep files from disk and shells
   out to git. Produces the plain data bb-depsolve.graph.dag consumes; performs no
   analysis of its own."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.graph.dag :as graph]
            [bb-depsolve.schema.api :as sch]
            [bb-depsolve.version.api :as v]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [bb-depsolve.core.skip :as skip]
            [bb-depsolve.core.resolve :as resolve]
            [hive-dsl.result :as r]))

(defn publishable-dirs
  "Git-initialized subdirectories of ROOT-DIR carrying a version.edn.
   Returns a sorted vector of absolute paths.

   The workspace skip file is unioned onto SKIP-DIRS, and a linked worktree of
   a sibling checkout is left out: it is a branch of that library, not a
   library of its own (bb-depsolve.core.discovery/excluded-dir?)."
  [root-dir skip-dirs]
  (let [skip-dirs (into (set skip-dirs) (skip/dirs root-dir))]
    (->> (fs/list-dir root-dir)
         (filter fs/directory?)
         (remove #(discovery/excluded-dir? root-dir skip-dirs %))
         (filter #(fs/exists? (fs/path % "version.edn")))
         (filter #(fs/exists? (fs/path % ".git")))
         (sort)
         (vec))))

(defn read-version-config
  "Parse PROJECT-DIR's version.edn. Returns nil when absent or unreadable."
  [project-dir]
  (let [f (fs/path project-dir "version.edn")]
    (when (fs/exists? f)
      (try (edn/read-string (slurp (str f)))
           (catch Exception _ nil)))))

(defn publish-target
  "The `:publish` target PROJECT-DIR's version.edn declares (:clojars, :gitea,
   :gitea-source, :none), or nil when the checkout or the key is absent."
  [project-dir]
  (:publish (read-version-config project-dir)))

(defn release-mode
  "Release model of PROJECT-DIR: :pinned when a tracked VERSION file drives the
   version, :rolling when it is derived from version.edn :minor plus the commit
   count."
  [project-dir]
  (if (fs/exists? (fs/path project-dir "VERSION")) :pinned :rolling))

(defn- git-count
  "Commit count from `git rev-list <rev> --count` in PROJECT-DIR.
   Returns nil when git fails."
  [project-dir rev]
  (let [{:keys [exit out]} (git/git project-dir "rev-list" rev "--count")]
    (when (zero? exit)
      (parse-long (str/trim (or out ""))))))

(defn project-version
  "Current version of PROJECT-DIR under MODE.
   :pinned reads VERSION; :rolling derives 0.{minor}.{commit-count}.
   Returns nil when the version cannot be determined."
  [project-dir cfg mode]
  (if (= :pinned mode)
    (let [f (fs/path project-dir "VERSION")]
      (when (fs/exists? f) (str/trim (slurp (str f)))))
    (when-let [n (git-count project-dir "HEAD")]
      (format "0.%s.%s" (:minor cfg 0) n))))

(defn latest-tag-version
  "Maven-form version of PROJECT-DIR's newest local semver tag, or nil."
  [project-dir]
  (let [result (resolve/resolve-local-tags project-dir)]
    (when (r/ok? result)
      (some-> (v/latest-tag (:ok result)) :tag v/tag->mvn-version))))

(defn release-base
  "The version a :pinned project last released: the higher of its declared
   VERSION and its newest tag. VERSION is a floor, not the truth: tag-released
   repos (CI tags without committing VERSION back) run ahead of it. Nil-safe."
  [version tag-version]
  (last (sort v/version-compare (remove nil? [version tag-version]))))

(defn commits-unreleased
  "Commits PROJECT-DIR has not published yet.
   :pinned counts commits past the tag of its release base (the higher of
   VERSION and the newest tag); :rolling counts commits not yet pushed to the
   tracking branch. Returns 0 when it cannot be determined."
  ([project-dir mode version] (commits-unreleased project-dir mode version nil))
  ([project-dir mode version tag-version]
   (or (if (= :pinned mode)
         (when-let [base (release-base version tag-version)]
           (git/git-commits-ahead project-dir (str "v" base)))
         (git-count project-dir "@{u}..HEAD"))
       0)))

(defn collect-nodes
  "Graph nodes for every publishable project under ROOT-DIR.
   ORG, when given, keeps only libs belonging to that GitHub org.
   Projects whose version.edn declares no :lib are skipped. A :pinned node
   carries :tag-version, its newest local semver tag, when it has one."
  [root-dir skip-dirs org]
  (->> (publishable-dirs root-dir skip-dirs)
       (keep (fn [dir]
               (let [cfg (read-version-config dir)
                     lib (:lib cfg)]
                 (when (and lib (or (nil? org) (v/lib-matches-org? org lib)))
                   (let [mode (release-mode dir)
                         tag-version (when (= :pinned mode) (latest-tag-version dir))]
                     (cond-> {:project (str (fs/file-name dir))
                              :lib lib
                              :dir (str dir)
                              :release-mode mode
                              :version (project-version dir cfg mode)}
                       tag-version (assoc :tag-version tag-version)))))))
       (vec)))

(defn collect-pins
  "Pins the DEP-FILES hold on libs published by NODES.
   Each pin carries :scope — :runtime for a top-level :deps coordinate, :alias
   for one declared under an alias or bb task — and :repos, the `:mvn/repos`
   its dep file declares, which is what the pinning project can fetch from.
   Coordinates naming an unknown lib are dropped; shadow-cljs files are skipped."
  [dep-files nodes]
  (let [by-lib (into {} (map (juxt (comp str :lib) :project)) nodes)]
    (->> dep-files
         (remove discovery/shadow-deps-file?)
         (mapcat (fn [{:keys [path project]}]
                   (let [content (slurp path)
                         repos (v/declared-repos content)
                         runtime (into #{} (map str) (v/runtime-libs content))
                         scope-of #(if (contains? runtime (str %)) :runtime :alias)]
                     (concat
                      (for [{:keys [lib tag]} (v/find-git-deps content)]
                        {:project project :lib lib :coord :git :version tag
                         :path path :scope (scope-of lib) :repos repos})
                      (for [{:keys [lib version]} (v/find-mvn-deps content)]
                        {:project project :lib lib :coord :mvn :version version
                         :path path :scope (scope-of lib) :repos repos})))))
         (keep (fn [{:keys [lib] :as pin}]
                 (when-let [dep (get by-lib (str lib))]
                   (assoc pin :dep dep))))
         (vec))))

(defn collect-unlinked-pins
  "Internal coordinates pinned by bare :git/sha in DEP-FILES.
   They name a lib published by NODES but carry no comparable version, so no
   edge is derived for them. Returns vec of {:project :dep :lib :sha :path}."
  [dep-files nodes]
  (let [by-lib (into {} (map (juxt (comp str :lib) :project)) nodes)]
    (->> dep-files
         (remove discovery/shadow-deps-file?)
         (mapcat (fn [{:keys [path project]}]
                   (for [{:keys [lib sha]} (v/find-git-sha-only-deps (slurp path))
                         :let [dep (get by-lib (str lib))]
                         :when dep]
                     {:project project :dep dep :lib lib :sha sha :path path})))
         (vec))))

(def default-edge-scopes
  "Pin scopes that constrain release ordering. Alias-scoped pins are recorded
   but do not order releases."
  #{:runtime})

(defn collect-graph
  "Internal dependency graph of the workspace at :root.
   OPTS: {:root :skip-dirs :depth :org :edge-scopes}.
   :edge-scopes selects which pin scopes induce ordering edges
   (default #{:runtime}). Nil options fall back to defaults.
   Validated at this boundary."
  [{:keys [root skip-dirs depth org edge-scopes]}]
  (let [skip-set (or skip-dirs discovery/default-skip-dirs)
        scopes (or edge-scopes default-edge-scopes)
        root-dir (str (fs/canonicalize (or root ".")))
        nodes (collect-nodes root-dir skip-set org)
        dep-files (discovery/find-dep-files {:root root-dir
                                        :skip-dirs skip-set
                                        :depth (or depth discovery/default-depth)})
        pins (collect-pins dep-files nodes)]
    (sch/validate! :bb-depsolve/internal-graph
                   (graph/dep-graph nodes pins
                                    {:edge? #(contains? scopes (:scope %))}))))

(defn detect-seeds
  "Projects among NODES carrying commits they have not published, counted
   from each project's release base rather than its possibly stale VERSION.
   Returns a sorted set of project names."
  [nodes]
  (into (sorted-set)
        (keep (fn [{:keys [project dir release-mode version tag-version]}]
                (when (pos? (commits-unreleased dir release-mode version tag-version))
                  project)))
        nodes))
