(ns bb-depsolve.core.remote
  "Which forge each workspace repo's first remote lives on.

   Answers {repo-dir => provider} with provider one of :github :gitea :gitlab
   :bitbucket :other. M3.2 push routing and M4 PR routing read this instead of
   guessing from a URL themselves.

   Host overrides are recorded by the workspace, at its root:

     .bb-depsolve.edn
     {:remote-providers {\"git.example.org\" :gitea
                         \"code.corp.example\" :gitlab}}

   An override outranks the built-in host heuristics, so a self-hosted forge
   on an unrecognisable hostname is classified by what it is, not by its name.

   The classification itself is a pure function over URL strings
   (bb-depsolve.core.git/remote-provider); reading git and the config file are
   the only effects, and both are injected into `classify-repos`."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.git :as git]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def config-file-name ".bb-depsolve.edn")

(def providers
  "The providers an override may name."
  #{:github :gitea :gitlab :bitbucket :other})

;; =============================================================================
;; Config (pure parse, boundary read)
;; =============================================================================

(defn normalize-overrides
  "Raw :remote-providers value -> {lowercase-host provider-keyword}.
   Hosts may be strings, keywords or symbols; providers may be keywords or
   strings. Entries naming no host or an unknown provider are dropped, so a
   typo in the config cannot turn a repo into a nonsense provider."
  [raw]
  (if-not (map? raw)
    {}
    (into {}
          (keep (fn [[host provider]]
                  (let [host     (some-> (if (or (keyword? host) (symbol? host))
                                           (name host)
                                           host)
                                         str str/trim str/lower-case not-empty)
                        provider (cond
                                   (keyword? provider) provider
                                   (string? provider)  (keyword (str/trim provider))
                                   :else               nil)]
                    (when (and host (contains? providers provider))
                      [host provider]))))
          raw)))

(defn config-path
  "Where the config file lives under ROOT."
  [root]
  (str (fs/path root config-file-name)))

(defn read-overrides
  "The normalized host overrides recorded under ROOT, or {} when there is no
   config file, it is unreadable, or it holds no :remote-providers."
  [root]
  (let [p (config-path root)]
    (if (fs/exists? p)
      (try
        (normalize-overrides (:remote-providers (edn/read-string (slurp p))))
        (catch Exception _ {}))
      {})))

;; =============================================================================
;; Classification
;; =============================================================================

(defn classify-repos
  "{repo-dir => provider} for REPO-DIRS. URL-FN maps a repo dir to its first
   remote URL (or nil); OVERRIDES is a normalized host->provider map. A repo
   with no remote classifies as :other."
  [repo-dirs url-fn overrides]
  (into {}
        (map (fn [dir] [(str dir) (git/remote-provider (url-fn dir) overrides)]))
        repo-dirs))

(defn remote-providers
  "{repo-dir => provider} for every git project with a VERSION file under
   ROOT (the same set the wave commands sweep), honouring ROOT's
   .bb-depsolve.edn :remote-providers. SKIP-DIRS is passed to discovery."
  ([root] (remote-providers root #{}))
  ([root skip-dirs]
   (classify-repos (discovery/find-workspace-projects root skip-dirs)
                   git/git-first-remote-url
                   (read-overrides root))))
