(ns bb-depsolve.core.auth
  "Forge credentials and registry endpoints."
  (:require [bb-depsolve.core.registry :as registry]))

(defn- basic
  "HTTP Basic Authorization header for USERNAME/PASSWORD, or nil when either
   is missing."
  [username password]
  (when (and username password)
    {"Authorization"
     (str "Basic "
          (.encodeToString (java.util.Base64/getEncoder)
                           (.getBytes (str username ":" password))))}))

(def ^:private discovered
  "The private registry found in the workspace, installed by `use-workspace!`.
   Read through `private-registry` at call time, never captured at wiring time."
  (atom nil))

(defn use-workspace!
  "Discover the private Maven registry the workspace under OPTS declares and
   install it as the fallback for `private-registry`. Returns the registry, or
   nil when the workspace declares none we hold credentials for."
  [opts]
  (reset! discovered (registry/discover opts)))

(defn private-registry
  "The private Maven registry in effect, as {:url :username :password}, or nil.

   MAVEN_URL wins outright: with it set the environment alone describes the
   registry, so an explicit override never half-merges with a discovered one.
   Otherwise the registry discovered from the workspace's `:mvn/repos` plus
   Maven's settings.xml answers."
  []
  (if-let [url (System/getenv "MAVEN_URL")]
    {:url url
     :username (System/getenv "MAVEN_USERNAME")
     :password (System/getenv "MAVEN_TOKEN")}
    @discovered))

(defn auth-headers
  "Build HTTP Authorization headers for forges and registries.
   Boundary: reads env vars and the discovered registry. Returns a header map
   (empty if no creds).

   Env vars consulted by TARGET:
     :github   GITHUB_TOKEN              -> {Authorization \"token <t>\"}
     :gitlab   GITLAB_TOKEN              -> {PRIVATE-TOKEN <t>}
     :codeberg CODEBERG_TOKEN            -> {Authorization \"token <t>\"}
     :clojars  CLOJARS_USERNAME+_PASSWORD-> {Authorization \"Basic <b64>\"}
     :gitea    MAVEN_USERNAME+MAVEN_TOKEN-> {Authorization \"Basic <b64>\"}
               falling back to the credentials settings.xml holds for the
               workspace's own private repository
     :maven    MAVEN_AUTH (raw header)   -> {Authorization <raw>}
     other                               -> {}"
  [target]
  (case target
    :github   (when-let [t (System/getenv "GITHUB_TOKEN")]
                {"Authorization" (str "token " t)})
    :gitlab   (when-let [t (System/getenv "GITLAB_TOKEN")]
                {"PRIVATE-TOKEN" t})
    :codeberg (when-let [t (System/getenv "CODEBERG_TOKEN")]
                {"Authorization" (str "token " t)})
    :clojars  (basic (System/getenv "CLOJARS_USERNAME")
                     (System/getenv "CLOJARS_PASSWORD"))
    :gitea    (let [{:keys [username password]} (private-registry)]
                (basic username password))
    :maven    (when-let [t (System/getenv "MAVEN_AUTH")]
                {"Authorization" t})
    {}))

(def github-url "https://github.com/%s/%s")

(defn gitea-registry-url
  "Base URL of the private Maven registry, or nil.
   MAVEN_URL wins; otherwise the workspace's own `:mvn/repos` declaration
   answers, provided settings.xml carries credentials for its repository id.
   nil disables the private registry as a resolution source."
  []
  (:url (private-registry)))
