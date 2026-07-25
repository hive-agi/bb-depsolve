(ns bb-depsolve.core.resolve
  "Registry resolution: tags and latest published versions."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :as proc]
            [bb-depsolve.core.auth :as auth]
            [bb-depsolve.core.fetch :as fetch]
            [bb-depsolve.schema :as sch]
            [bb-depsolve.version :as v]
            [cheshire.core :as json]
            [clojure.string :as str]
            [hive-dsl.bounded-atom :as ba]
            [hive-dsl.result :as r]))

(defn resolve-local-tags
  "Resolve all tags from a local git repo. Returns Result<[{:tag :sha :sha-short}]>.
   Annotated tags report the commit behind them, which is what tools.deps
   validates a :git/sha against."
  [repo-dir]
  (r/try-effect*
   :io/git-local-tags
   (let [result (proc/sh ["git" "-C" (str repo-dir) "tag" "--sort=-version:refname"
                          "-l" "v*"
                          "--format=%(refname:short)%09%(*objectname)%09%(objectname)"])]
     (if (zero? (:exit result))
       (v/parse-local-tag-output (:out result))
       (throw (ex-info "git tag failed" {:exit (:exit result)}))))))

(defn resolve-remote-tags
  "Resolve tags from a GitHub remote without cloning.
   Returns Result<[{:tag :sha :sha-short}]>. Annotated tags report their peeled
   commit, which is what tools.deps validates a :git/sha against."
  [org repo]
  (r/try-effect*
   :io/git-ls-remote
   (let [url (format auth/github-url org repo)
         result (proc/sh ["git" "ls-remote" "--tags" "--sort=-version:refname" url])]
     (if (zero? (:exit result))
       (v/parse-ls-remote-tags (:out result))
       (throw (ex-info "git ls-remote failed" {:exit (:exit result)}))))))

(defn resolve-lib-tags
  "Resolve the latest tag+sha for a git lib.
   Uses GitHub remote first, falls back to local clone.
   Returns Result<{:tag :sha :source}>. The resolved value is
   schema-validated at this boundary (fail-loud)."
  [root-dir lib-sym dir-name]
  (if-let [{:keys [org repo]} (v/parse-github-lib lib-sym)]
    (let [remote-result (resolve-remote-tags org repo)]
      (if (and (r/ok? remote-result) (seq (:ok remote-result)))
        (if-let [latest (v/latest-tag (:ok remote-result))]
          (r/ok (assoc (sch/validate! :bb-depsolve/resolved-lib latest) :source :remote))
          (r/err :parse/no-semver-tags {:lib lib-sym}))
        (let [local-dir (fs/path root-dir dir-name)]
          (if (fs/directory? (fs/path local-dir ".git"))
            (r/let-ok [tags (resolve-local-tags local-dir)]
                      (if-let [latest (v/latest-tag tags)]
                        (r/ok (assoc (sch/validate! :bb-depsolve/resolved-lib latest) :source :local))
                        (r/err :parse/no-semver-tags {:lib lib-sym})))
            (r/err :io/not-found {:lib lib-sym :dir (str local-dir)})))))
    (r/err :parse/not-github-lib {:lib lib-sym})))

(defn resolve-clojars-latest
  "Query Clojars API for latest release version. Returns Result<string>.
   Honors CLOJARS_USERNAME/CLOJARS_PASSWORD env for private repos."
  [group-id artifact-id]
  (r/try-effect*
   :io/clojars
   (let [url (format "https://clojars.org/api/artifacts/%s/%s" group-id artifact-id)
         headers (merge {"Accept" "application/json"} (auth/auth-headers :clojars))
         resp (http/get url {:headers headers :throw false})]
     (if (= 200 (:status resp))
       (or (-> (json/parse-string (:body resp) true) :latest_release)
           (throw (ex-info "No latest_release" {:group group-id :artifact artifact-id})))
       (throw (ex-info "Clojars HTTP error" {:status (:status resp)}))))))

(defn resolve-maven-latest
  "Query Maven Central for latest version. Returns Result<string>.
   Honors MAVEN_AUTH env var (raw Authorization header)."
  [group-id artifact-id]
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
       (throw (ex-info "Maven HTTP error" {:status (:status resp)}))))))

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

(defn maven-cache-dir
  "Local Maven repository used for cached remote metadata. M2_REPO overrides
   ~/.m2/repository."
  []
  (or (System/getenv "M2_REPO")
      (str (fs/path (System/getProperty "user.home") ".m2" "repository"))))

(defn resolve-cached-maven-latest
  "Resolve latest version from Maven's cached REMOTE metadata.
   Ignores maven-metadata-local.xml so locally installed/deployed-only versions
   cannot masquerade as published artifacts. Returns Result<string>."
  [group artifact allow-pre?]
  (r/try-effect*
   :io/maven-cache
   (let [artifact-dir (fs/path (maven-cache-dir) (v/group-id->path group) artifact)
         metadata-files (when (fs/directory? artifact-dir)
                          (->> (fs/glob artifact-dir "maven-metadata-*.xml")
                               (remove #(= "maven-metadata-local.xml" (str (fs/file-name %))))))
         versions (->> metadata-files
                       (mapcat #(v/parse-maven-metadata-versions (slurp (str %))))
                       (filter #(or allow-pre? (not (v/pre-release? %)))))]
     (or (last (sort v/version-compare versions))
         (throw (ex-info "No cached remote Maven metadata version"
                         {:group group :artifact artifact}))))))

(defn resolve-clojars-versions
  "Every version Clojars lists for the artifact. Returns Result<#{string}>."
  [group-id artifact-id]
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
       (throw (ex-info "Clojars HTTP error" {:status (:status resp)}))))))

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

(defn resolve-mvn-latest
  "Resolve latest PUBLISHED version across registries. Returns Result<string>.

   Always consults Clojars (falling back to Maven Central). When MAVEN_URL is
   set, ALSO consults the private Gitea Maven registry plus Maven's cached
   remote metadata, returning the NEWEST version across whichever sources
   succeed (compared via version-compare).
   Filters pre-releases unless allow-pre? is true. Errs when no source yields
   an acceptable published version."
  [lib-sym allow-pre?]
  (let [[group artifact] (str/split (str lib-sym) #"/")
        group (or group artifact)
        artifact (or artifact group)
        gitea-base (auth/gitea-registry-url)
        candidates (cond-> [(let [clojars (resolve-clojars-latest group artifact)]
                              (if (r/ok? clojars)
                                clojars
                                (resolve-maven-latest group artifact)))]
                     gitea-base (conj (resolve-gitea-latest gitea-base group artifact allow-pre?)
                                      (resolve-cached-maven-latest group artifact allow-pre?)))
        versions (->> candidates
                      (filter r/ok?)
                      (map :ok)
                      (remove nil?)
                      (filter #(or allow-pre? (not (v/pre-release? %)))))]
    (if (seq versions)
      (r/ok (last (sort v/version-compare versions)))
      (r/err :io/no-published-version {:lib lib-sym :allow-pre? allow-pre?}))))

(defn resolve-dep-children
  "Resolve children for a dep. Dispatches by lib type. Uses bounded cache.
   Returns [{:lib :version :type}]."
  [cache lib version]
  (let [key [lib version]]
    (if-let [cached (ba/bget cache key)]
      cached
      (let [lib-str (str lib)
            [group artifact] (str/split lib-str #"/" 2)
            group (or group artifact)
            artifact (or artifact group)
            result (let [resolved (if-let [{:keys [org repo]} (v/parse-github-lib lib)]
                                    (fetch/fetch-git-dep-coords org repo version)
                                    (fetch/fetch-pom-deps group artifact version))]
                     (if (r/ok? resolved)
                       (mapv #(assoc % :type (or (:type %) :mvn)) (:ok resolved))
                       []))]
        (ba/bput! cache key result)
        result))))