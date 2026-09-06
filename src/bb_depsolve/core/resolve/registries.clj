(ns bb-depsolve.core.resolve.registries
  (:require [babashka.http-client :as http]
            [bb-depsolve.core.auth :as auth]
            [bb-depsolve.version.api :as v]
            [cheshire.core :as json]
            [clojure.string :as str]
            [hive-dsl.result :as r]))

(declare resolve-clojars-versions resolve-clojars-latest resolve-maven-versions resolve-maven-latest resolve-gitea-versions resolve-gitea-latest resolve-mvn-versions read-registry-latest resolve-mvn-reads resolve-mvn-by-registry resolve-mvn-latest)

(def ^:private maven-central-base
  "Maven Central's repository root — the authoritative artifact listing."
  "https://repo1.maven.org/maven2")

(def ^:private clojars-repo-base
  "Clojars' repository root — the authoritative artifact listing."
  "https://repo.clojars.org")

(defn resolve-clojars-versions
  "Every version Clojars lists for the artifact. Returns Result<#{string}>.

   Reads repo.clojars.org's maven-metadata.xml — the complete listing. The JSON
   API is a FALLBACK: it reports `latest_release` plus at most five
   `recent_versions`, so a lib with a longer history is under-reported."
  [group-id artifact-id]
  (let [metadata (r/try-effect*
                  :io/clojars
                  (let [url (v/maven-metadata-url clojars-repo-base group-id artifact-id)
                        headers (merge {"Accept" "application/xml"} (auth/auth-headers :clojars))
                        resp (http/get url {:headers headers :throw false})]
                    (if (= 200 (:status resp))
                      (:body resp)
                      (throw (ex-info "Clojars metadata HTTP error" {:status (:status resp)})))))
        versions (when (r/ok? metadata)
                   (seq (v/parse-maven-metadata-versions (:ok metadata))))]
    (if versions
      (r/ok (set versions))
      (r/try-effect*
       :io/clojars
       (let [url (format "https://clojars.org/api/artifacts/%s/%s" group-id artifact-id)
             headers (merge {"Accept" "application/json"} (auth/auth-headers :clojars))
             resp (http/get url {:headers headers :throw false})]
         (if (= 200 (:status resp))
           (let [body (json/parse-string (:body resp) true)]
             (into (if-let [latest (:latest_release body)] #{latest} #{})
                   (keep :version)
                   (:recent_versions body)))
           (throw (ex-info "Clojars HTTP error" {:status (:status resp)}))))))))

(defn resolve-clojars-latest
  "Latest version Clojars publishes for the artifact. Returns Result<string>.

   Reads repo.clojars.org's maven-metadata.xml — the repository's own listing —
   and takes the newest entry by version-compare, skipping pre-releases unless
   allow-pre?. The JSON API's `latest_release` is a FALLBACK only: it names the
   newest UPLOAD, pre-releases included (it reported metosin/reitit 0.11.0-rc1
   and http-kit 2.9.0-beta4), so a resolver reading it loses the library
   entirely once a maintainer cuts a release candidate.

   Honors CLOJARS_USERNAME/CLOJARS_PASSWORD env for private repos."
  ([group-id artifact-id] (resolve-clojars-latest group-id artifact-id false))
  ([group-id artifact-id allow-pre?]
   (let [metadata (r/try-effect*
                   :io/clojars
                   (let [url (v/maven-metadata-url clojars-repo-base group-id artifact-id)
                         headers (merge {"Accept" "application/xml"} (auth/auth-headers :clojars))
                         resp (http/get url {:headers headers :throw false})]
                     (if (= 200 (:status resp))
                       (:body resp)
                       (throw (ex-info "Clojars metadata HTTP error" {:status (:status resp)})))))
         latest (when (r/ok? metadata)
                  (v/latest-published-version (:ok metadata) {:allow-pre? allow-pre?}))]
     (if latest
       (r/ok latest)
       (r/try-effect*
        :io/clojars
        (let [url (format "https://clojars.org/api/artifacts/%s/%s" group-id artifact-id)
              headers (merge {"Accept" "application/json"} (auth/auth-headers :clojars))
              resp (http/get url {:headers headers :throw false})]
          (if (= 200 (:status resp))
            (let [release (-> (json/parse-string (:body resp) true) :latest_release)]
              (if (and release (or allow-pre? (not (v/pre-release? release))))
                release
                (throw (ex-info "No published latest_release"
                                {:group group-id :artifact artifact-id :latest-release release}))))
            (throw (ex-info "Clojars HTTP error" {:status (:status resp)})))))))))

(defn resolve-maven-versions
  "Every version Maven Central lists for the artifact. Returns Result<#{string}>."
  [group-id artifact-id]
  (r/try-effect*
   :io/maven-central
   (let [url (format "https://search.maven.org/solrsearch/select?q=g:%%22%s%%22+AND+a:%%22%s%%22&core=gav&rows=200&wt=json"
                     group-id artifact-id)
         headers (auth/auth-headers :maven)
         resp (http/get url (merge {:throw false}
                                   (when (seq headers) {:headers headers})))]
     (if (= 200 (:status resp))
       (into #{} (keep :v) (-> (json/parse-string (:body resp) true) :response :docs))
       (throw (ex-info "Maven HTTP error" {:status (:status resp)}))))))

(defn resolve-maven-latest
  "Latest version Maven Central publishes for the artifact. Returns Result<string>.

   Reads repo1's maven-metadata.xml — the repository's own listing — and takes
   the newest entry by version-compare, skipping pre-releases unless allow-pre?.
   search.maven.org is a FALLBACK only: its `latestVersion` is a denormalized
   Solr field that lags the repository (it reported postgresql 42.7.7 while
   repo1 already listed 42.7.13), so a resolver reading it silently
   under-reports upgrades.

   Honors MAVEN_AUTH env var (raw Authorization header)."
  ([group-id artifact-id] (resolve-maven-latest group-id artifact-id false))
  ([group-id artifact-id allow-pre?]
   (let [metadata (r/try-effect*
                   :io/maven-central
                   (let [url (v/maven-metadata-url maven-central-base group-id artifact-id)
                         headers (merge {"Accept" "application/xml"} (auth/auth-headers :maven))
                         resp (http/get url {:headers headers :throw false})]
                     (if (= 200 (:status resp))
                       (:body resp)
                       (throw (ex-info "Maven metadata HTTP error" {:status (:status resp)})))))
         latest (when (r/ok? metadata)
                  (v/latest-published-version (:ok metadata) {:allow-pre? allow-pre?}))]
     (if latest
       (r/ok latest)
       (r/try-effect*
        :io/maven-central
        (let [url (format "https://search.maven.org/solrsearch/select?q=g:%%22%s%%22+AND+a:%%22%s%%22&rows=1&wt=json"
                          group-id artifact-id)
              headers (auth/auth-headers :maven)
              resp (http/get url (merge {:throw false}
                                        (when (seq headers) {:headers headers})))]
          (if (= 200 (:status resp))
            (or (-> (json/parse-string (:body resp) true) :response :docs first :latestVersion)
                (throw (ex-info "No latestVersion" {:group group-id :artifact artifact-id})))
            (throw (ex-info "Maven HTTP error" {:status (:status resp)})))))))))

(defn resolve-gitea-versions
  "Every version the private Gitea Maven registry lists. Returns Result<#{string}>."
  [base-url group artifact]
  (r/try-effect*
   :io/gitea
   (let [url (v/maven-metadata-url base-url group artifact)
         headers (merge {"Accept" "application/xml"} (auth/auth-headers :gitea))
         resp (http/get url {:headers headers :throw false})]
     (if (= 200 (:status resp))
       (set (v/parse-maven-metadata-versions (:body resp)))
       (throw (ex-info "Gitea HTTP error" {:status (:status resp)}))))))

(defn resolve-gitea-latest
  "Resolve latest PUBLISHED version from the private Gitea Maven registry.
   Returns Result<string>. GETs maven-metadata.xml (with :gitea auth), then
   takes MAX of <versions> (honoring allow-pre?). Errs when no version found."
  [base-url group artifact & [allow-pre?]]
  (r/try-effect*
   :io/gitea
   (let [url (v/maven-metadata-url base-url group artifact)
         headers (merge {"Accept" "application/xml"} (auth/auth-headers :gitea))
         resp (http/get url {:headers headers :throw false})]
     (if (= 200 (:status resp))
       (or (v/latest-published-version (:body resp) {:allow-pre? allow-pre?})
           (throw (ex-info "No published version in Gitea metadata"
                           {:group group :artifact artifact})))
       (throw (ex-info "Gitea HTTP error" {:status (:status resp)}))))))

(defn resolve-mvn-versions
  "Union of every version LIB-SYM resolves to across the maven registries.
   Unreachable registries contribute nothing rather than failing the union.
   Pre-releases are dropped unless ALLOW-PRE?. Returns #{string}."
  [lib-sym allow-pre?]
  (let [[group artifact] (str/split (str lib-sym) #"/")
        group (or group artifact)
        artifact (or artifact group)
        gitea-base (auth/gitea-registry-url)
        results (cond-> [(resolve-clojars-versions group artifact)
                         (resolve-maven-versions group artifact)]
                  gitea-base (conj (resolve-gitea-versions gitea-base group artifact)))
        versions (into #{} (comp (filter r/ok?) (mapcat :ok)) results)]
    (if allow-pre?
      versions
      (into #{} (remove v/pre-release?) versions))))

(defn- split-lib
  "[group artifact] of LIB-SYM; an unqualified name is both."
  [lib-sym]
  (let [[group artifact] (str/split (str lib-sym) #"/")]
    [(or group artifact) (or artifact group)]))

(defn- registry-host
  "Host of URL, the label a private registry gets when it has no repository id."
  [url]
  (try (.getHost (java.net.URI. (str url)))
       (catch Exception _ nil)))

(defn- http-metadata
  "GET a maven-metadata.xml. Returns the response map, or {:status nil
   :message m} when the transport itself failed (timeout, DNS, refused)."
  [url headers]
  (try
    (http/get url {:headers headers :throw false})
    (catch Exception e
      {:status nil :message (ex-message e)})))

(defn read-registry-latest
  "ONE registry's answer for an artifact, kept apart from a failure to answer:
     {:version v}   it lists an acceptable version (pre-releases dropped unless ALLOW-PRE?)
     {:absent true} it answered, and the artifact (or an acceptable version) is not there
     {:unread {:status :message}} it did not answer: transport failure, 5xx,
                    or an auth refusal (401/403), which is a blind read, not an absence.
   Reads BASE-URL's maven-metadata.xml with the credentials AUTH-TARGET names."
  [base-url auth-target group artifact allow-pre?]
  (let [url (v/maven-metadata-url base-url group artifact)
        headers (merge {"Accept" "application/xml"} (auth/auth-headers auth-target))
        {:keys [status body message]} (http-metadata url headers)]
    (cond
      (= 200 status) (if-let [latest (v/latest-published-version body {:allow-pre? allow-pre?})]
                       {:version latest}
                       {:absent true})
      (= 404 status) {:absent true}
      :else          {:unread (cond-> {:status status}
                                message (assoc :message message))})))

(defn resolve-mvn-reads
  "Every registry's answer for LIB-SYM, as
   {:versions [registry-version] :unread [registry-read-failure]}.

   Clojars is read first; Maven Central only when Clojars answers ABSENT (a
   Clojars that did not answer is reported unread, never papered over by
   Central or by a JSON API). The private registry is read when one is
   configured. Nothing else counts: the ~/.m2 cache is not a registry, and a
   stale cache standing in for a registry that did not answer is exactly how
   a plan full of downgrades gets written."
  [lib-sym allow-pre?]
  (let [[group artifact] (split-lib lib-sym)
        private-url (auth/gitea-registry-url)
        private-id (or (:id (auth/private-registry)) (registry-host private-url) "private")
        clojars (read-registry-latest clojars-repo-base :clojars group artifact allow-pre?)
        central (when (:absent clojars)
                  (read-registry-latest maven-central-base :maven group artifact allow-pre?))
        private (when private-url
                  (read-registry-latest private-url :gitea group artifact allow-pre?))
        reads [["clojars" clojars-repo-base true clojars]
               ["central" maven-central-base true central]
               [private-id private-url false private]]]
    {:versions (vec (for [[id url public? {:keys [version]}] reads
                          :when version]
                      {:id id :url url :public? public? :version version}))
     :unread   (vec (for [[id url public? {:keys [unread]}] reads
                          :when unread]
                      (cond-> {:id id :url url :public? public? :error :io/unread}
                        (:status unread)  (assoc :status (:status unread))
                        (:message unread) (assoc :message (:message unread)))))}))

(defn resolve-mvn-by-registry
  "Latest PUBLISHED version of LIB-SYM per registry that ANSWERED. Returns
   Result<[registry-version]>: one {:id :url :public? :version} per registry,
   sorted by id. Errs :io/registry-unread when nothing answered but some
   registry failed to (the read was blind), :io/no-published-version when
   every registry answered and none lists an acceptable version.

   A consumer sees a version only through the registries it declares, so the
   answer keeps them apart instead of collapsing to one MAX. See
   resolve-mvn-reads for the unread half."
  [lib-sym allow-pre?]
  (let [{:keys [versions unread]} (resolve-mvn-reads lib-sym allow-pre?)]
    (cond
      (seq versions) (r/ok (vec (sort-by :id versions)))
      (seq unread)   (r/err :io/registry-unread {:lib lib-sym :unread unread})
      :else          (r/err :io/no-published-version {:lib lib-sym :allow-pre? allow-pre?}))))

(defn resolve-mvn-latest
  "Resolve latest PUBLISHED version across registries. Returns Result<string>.

   This is the RESOLVER's view: the newest version ANY registry that answered
   lists. It is not what one consumer can fetch; for that, project
   resolve-mvn-by-registry through the consumer's declared repositories
   (bb-depsolve.version.repos). Filters pre-releases unless allow-pre?. Errs
   :io/registry-unread when no registry answered, :io/no-published-version
   when they all did and none lists an acceptable version."
  [lib-sym allow-pre?]
  (r/let-ok [by-registry (resolve-mvn-by-registry lib-sym allow-pre?)]
    (r/ok (:version (last (sort-by :version v/version-compare by-registry))))))
