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

;; =============================================================================
;; Push policy: which remotes push-all may touch (pure)
;; =============================================================================

(defn glob-match?
  "True when text S matches glob PATTERN, case-insensitively. `*` matches any
   run of characters (slashes included, so it spans a whole URL), `?` exactly
   one; every other character matches itself. Pure, no regex: a plain
   two-pointer scan that backtracks to the last `*`."
  [pattern s]
  (let [p (vec (str/lower-case (str pattern)))
        t (vec (str/lower-case (str s)))
        m (count p)
        n (count t)]
    (loop [pi 0 ti 0 star -1 mark 0]
      (if (< ti n)
        (cond
          (and (< pi m) (= (p pi) \*))
          (recur (inc pi) ti pi ti)

          (and (< pi m) (or (= (p pi) \?) (= (p pi) (t ti))))
          (recur (inc pi) (inc ti) star mark)

          (>= star 0)
          (recur (inc star) (inc mark) star (inc mark))

          :else false)
        (every? #{\*} (subvec p pi))))))

(defn parse-patterns
  "A --remotes / --skip-remote value or a config list -> vector of trimmed,
   non-blank glob patterns. Accepts a comma-separated string, a collection of
   strings/keywords/symbols, or nil. The bare flag (`true`) means every
   remote, i.e. [\"*\"]. Pure."
  [raw]
  (cond
    (true? raw)       ["*"]
    (string? raw)     (parse-patterns (str/split-lines (str/replace raw "," "\n")))
    (sequential? raw) (into []
                            (keep (fn [x]
                                    (some-> (if (or (keyword? x) (symbol? x)) (name x) x)
                                            str str/trim not-empty)))
                            raw)
    (set? raw)        (parse-patterns (vec raw))
    :else             []))

(defn normalize-push-policy
  "Raw :push-policy value -> {:include [pattern ...] :exclude [pattern ...]}.
   Both keys are always present; anything that is not a map is the empty
   policy, which lets every remote through. Pure."
  [raw]
  (let [raw (if (map? raw) raw {})]
    {:include (parse-patterns (:include raw))
     :exclude (parse-patterns (:exclude raw))}))

(defn read-push-policy
  "The normalized push policy recorded under ROOT's .bb-depsolve.edn
   :push-policy, or the empty policy when there is no config file, it is
   unreadable, or it holds no :push-policy."
  [root]
  (let [p (config-path root)]
    (if (fs/exists? p)
      (try
        (normalize-push-policy (:push-policy (edn/read-string (slurp p))))
        (catch Exception _ (normalize-push-policy nil)))
      (normalize-push-policy nil))))

(defn resolve-push-policy
  "The policy in force: each side the command line names (--remotes ->
   :include, --skip-remote -> :exclude) replaces the config's side; a side
   the command line leaves out falls back to CONFIG. CLI-OPTS is
   {:remotes raw :skip-remote raw}. Pure."
  [config {:keys [remotes skip-remote]}]
  (let [config (normalize-push-policy config)]
    {:include (if (some? remotes)     (parse-patterns remotes)     (:include config))
     :exclude (if (some? skip-remote) (parse-patterns skip-remote) (:exclude config))}))

(defn remote-target
  "What a push policy sees of one repo: {:url :host :provider}. PROVIDER is the
   classified forge keyword (OVERRIDES as for git/remote-provider). Pure."
  [url overrides]
  {:url      url
   :host     (git/remote-host url)
   :provider (git/remote-provider url overrides)})

(defn- target-matches?
  [pattern {:keys [url host provider]}]
  (boolean
   (some #(and % (glob-match? pattern %))
         [url host (some-> provider name)])))

(defn push-decision
  "Whether POLICY lets a repo whose remote is TARGET ({:url :host :provider})
   be pushed. A pattern matches the remote URL, its host, or its provider
   name (`gitea`, `gitea.*`, `*.corp.example`, `*://git.example.org/*`).
   :exclude outranks :include; a non-empty :include admits only what it
   matches. Returns {:push? bool :reason kw :pattern str-or-nil}, reason one of
   :allowed :excluded :not-included. Pure."
  [{:keys [include exclude]} target]
  (if-let [hit (first (filter #(target-matches? % target) exclude))]
    {:push? false :reason :excluded :pattern hit}
    (if (and (seq include) (not-any? #(target-matches? % target) include))
      {:push? false :reason :not-included :pattern nil}
      {:push? true :reason :allowed :pattern nil})))

(defn push-decisions
  "{repo-dir => push-decision} for REPO-DIRS under POLICY. URL-FN maps a repo
   dir to its first remote URL (or nil); OVERRIDES is a normalized
   host->provider map. A repo with no remote is always {:push? true
   :reason :no-remote}: there is nothing to exclude, and the push plan skips it
   on its own account. Pure given the injected URL-FN."
  [repo-dirs url-fn overrides policy]
  (into {}
        (map (fn [dir]
               (let [url (url-fn dir)]
                 [(str dir)
                  (if (nil? url)
                    {:push? true :reason :no-remote :pattern nil}
                    (push-decision policy (remote-target url overrides)))])))
        repo-dirs))

(defn workspace-push-policy
  "The push policy and host overrides in force for ROOT, as
   {:policy {:include :exclude} :overrides {host provider}}. CLI-OPTS is
   {:remotes raw :skip-remote raw}: each side it names replaces ROOT's
   .bb-depsolve.edn :push-policy side. Reads the config file."
  [root cli-opts]
  {:policy    (resolve-push-policy (read-push-policy root) cli-opts)
   :overrides (read-overrides root)})
