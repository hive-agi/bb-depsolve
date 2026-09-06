(ns bb-depsolve.core.parity
  "Registry parity: where a lib's artifacts are versus where it says they go.

   Calculation: `classify` turns a lib's declared publish target plus its
   per-registry versions into a finding, or nil when the registries agree.
   Boundary: `findings` reads each lib's version.edn from its sibling
   checkout; `print-findings!` names the remedy, including the git remote to
   push when a checkout is present.

   Contract: a public lib (publish :clojars) must have its newest version on
   the public registry, because a public consumer holds no credentials for the
   private one. When the private registry is ahead, the fix is to sync the
   forges (push the private state to the public GitHub repo so its release CI
   publishes), never to hand the private registry to a public consumer."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [malli.core :as m]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.graph.collect :as collect]
            [bb-depsolve.schema.api :as sch]
            [bb-depsolve.version.api :as v]
            [bb-depsolve.version.repos :as repos]))

;; ---------------------------------------------------------------- Calculation

(def public-targets
  "version.edn :publish values that mean the lib is public."
  #{:clojars})

(def private-targets
  "version.edn :publish values that mean the lib is private."
  #{:gitea :gitea-source})

(defn public-lib?
  "Whether a lib is public, from its EVIDENCE: a declared public target, or
   GitHub hosting (the publish target follows the hosting, so a GitHub repo
   is public whatever version.edn says), or, with neither declared, a public
   registry already listing it (PUBLIC is that registry-version or nil)."
  [{:keys [publish hosting]} public]
  (boolean
   (or (contains? public-targets publish)
       (= :github hosting)
       (and (nil? publish) (nil? hosting) (some? public)))))

(defn classify
  "The parity finding for LIB, or nil when its registries agree.
   EVIDENCE is what its checkout says: `:publish` from version.edn (nil when
   unknown) and `:hosting` (:github, :private, or nil). BY-REGISTRY is its
   registry-versions; UNREAD the registries that did not answer.

     :private-ahead    a public lib whose newest version is private-only.
     :unread           a registry did not answer, so nothing else can be
                       certified for this lib (a definite :private-ahead
                       from the registries that did answer still wins).
     :publicly-leaked  a lib declared private, not GitHub-hosted, that a
                       public registry lists.
     :declared-none    a lib declared :publish :none, not GitHub-hosted, that
                       a registry lists."
  ([lib evidence by-registry]
   (classify lib evidence by-registry []))
  ([lib {:keys [publish hosting] :as evidence} by-registry unread]
   (let [public (repos/newest (filter :public? by-registry))
         private (repos/newest (remove :public? by-registry))
         github? (= :github hosting)
         public-unread? (boolean (some :public? unread))
         ahead? (and private
                     (if public
                       (pos? (v/version-compare (:version private) (:version public)))
                       (not public-unread?)))
         finding (fn [kind]
                   (cond-> {:lib lib :kind kind :publish publish :public public :private private}
                     hosting (assoc :hosting hosting)))]
     (cond
       (and (public-lib? evidence public) ahead?)                          (finding :private-ahead)
       (seq unread)                                                        (assoc (finding :unread) :unread (vec unread))
       (and (contains? private-targets publish) (not github?) public)      (finding :publicly-leaked)
       (and (= :none publish) (not github?) (seq by-registry))             (finding :declared-none)
       :else nil))))

(defn classify-all
  "Findings for every lib in RESOLVED with registry detail (:mvn-by-registry
   or :mvn-unread). EVIDENCE-OF maps a lib to its evidence map. Sorted by lib."
  [resolved evidence-of]
  (->> resolved
       (keep (fn [[lib {:keys [mvn-by-registry mvn-unread]}]]
               (when (or (seq mvn-by-registry) (seq mvn-unread))
                 (classify lib (or (evidence-of lib) {}) (or mvn-by-registry []) (or mvn-unread [])))))
       (sort-by (comp str :lib))
       vec))

(defn blocking?
  "True when FINDINGS carry a kind that leaves parity uncertified: a public
   consumer unable to fetch what the private registry has, or a registry that
   did not answer."
  [findings]
  (boolean (some #(contains? #{:private-ahead :unread} (:kind %)) findings)))

(m/=> classify
      [:function
       [:=> [:cat :bb-depsolve/lib :bb-depsolve/lib-evidence [:sequential :bb-depsolve/registry-version]]
        [:maybe :bb-depsolve/parity-finding]]
       [:=> [:cat :bb-depsolve/lib :bb-depsolve/lib-evidence [:sequential :bb-depsolve/registry-version]
             [:sequential :bb-depsolve/registry-read-failure]]
        [:maybe :bb-depsolve/parity-finding]]])

;; ------------------------------------------------------------------- Boundary

(defn lib-dir
  "Sibling checkout of LIB under ROOT-DIR, or nil when absent."
  [root-dir lib]
  (let [dir (fs/path root-dir (v/lib-artifact-id lib))]
    (when (fs/directory? dir) (str dir))))

(defn github-remote
  "Name of the first git remote of PROJECT-DIR whose push url points at
   github.com, or nil."
  [project-dir]
  (let [{:keys [exit out]} (git/git project-dir "remote" "-v")]
    (when (zero? exit)
      (->> (str/split-lines (or out ""))
           (keep (fn [line]
                   (let [[nm url kind] (str/split (str/trim line) #"\s+")]
                     (when (and url (str/includes? url "github.com") (= kind "(push)"))
                       nm))))
           first))))

(defn evidence
  "What the sibling checkout of LIB under ROOT-DIR says about it: the
   `:publish` target its version.edn declares, and its `:hosting` (:github
   when a remote points at github.com, :private when its remotes are all
   elsewhere, absent without remotes). {} without a checkout."
  [root-dir lib]
  (if-let [dir (lib-dir root-dir lib)]
    (cond-> {:publish (collect/publish-target dir)}
      (github-remote dir)      (assoc :hosting :github)
      (and (not (github-remote dir)) (git/git-has-remote? dir)) (assoc :hosting :private))
    {}))

(defn findings
  "Parity findings for RESOLVED, reading each lib's evidence from its sibling
   checkout under ROOT-DIR. Validated at this boundary."
  [root-dir resolved]
  (mapv #(sch/validate! :bb-depsolve/parity-finding %)
        (classify-all resolved #(evidence root-dir %))))

(defn- remedy-lines
  "What to do about one finding, as printable lines."
  [root-dir {:keys [lib kind private publish unread]}]
  (case kind
    :private-ahead
    (let [dir (lib-dir root-dir lib)
          rel (when dir (str (fs/relativize root-dir dir)))
          remote (when dir (github-remote dir))]
      (cond-> [(str "public consumers cannot fetch " (:version private)
                    ". Sync the forges: push the private state to GitHub so its"
                    " release CI publishes it publicly:")]
        remote            (conj (str "  git -C " rel " push " remote " HEAD:main"))
        (and dir (not remote)) (conj (str "  no github remote in " rel "; add one, then push HEAD:main"))
        (not dir)         (conj "  no checkout here; push the private repo's default branch to the public GitHub repo")
        (not (contains? public-targets publish))
        (conj (str "  version.edn declares " (if publish (str ":publish " publish) "no :publish")
                   "; the publish target follows the hosting, so set :publish :clojars"))))
    :unread
    [(str "did not answer: "
          (str/join ", " (map (fn [{:keys [id status message]}]
                                (str id " (" (or (some->> status (str "HTTP ")) message "no response") ")"))
                              unread))
          ". A blind read certifies nothing; re-run when the registry answers.")]
    :publicly-leaked
    ["declared private, yet a public registry lists it: set :publish to where it really goes"]
    :declared-none
    ["declared :publish :none, yet registries list it: declare the target once and enforce it at both ends"]))

(defn- registry-cell
  [{:keys [id version]}]
  (if id (str id " " version) "none"))

(defn print-findings!
  "Print FINDINGS with their remedies. Action: prints."
  [root-dir findings]
  (println (ui/c :yellow (format "Registry parity: %d finding(s)" (count findings))))
  (doseq [{:keys [lib kind public private unread] :as finding} findings
          :let [public-unread? (some :public? unread)
                private-unread? (some (complement :public?) unread)]]
    (printf "  %-40s %s  %s  %s\n"
            (ui/c :cyan (str lib))
            (cond
              public-unread?              (ui/c :red "unread")
              (= kind :publicly-leaked)   (ui/c :yellow (registry-cell public))
              :else                       (ui/c :green (registry-cell public)))
            (case kind :private-ahead "<" :unread "?" :publicly-leaked "!=" :declared-none "?")
            (cond
              private-unread?             (ui/c :red "unread")
              (= kind :private-ahead)     (ui/c :yellow (registry-cell private))
              :else                       (ui/c :dim (registry-cell private))))
    (doseq [line (remedy-lines root-dir finding)]
      (println (ui/c :dim (str "      " line))))))
