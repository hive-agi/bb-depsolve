(ns bb-depsolve.core.auth
  "Forge credentials and registry endpoints.")

(defn auth-headers
  "Build HTTP Authorization headers for forges and registries from env vars.
   Pure-ish at boundary: reads env vars only. Returns a header map (empty if no creds).

   Env vars consulted by TARGET:
     :github   GITHUB_TOKEN              -> {Authorization \"token <t>\"}
     :gitlab   GITLAB_TOKEN              -> {PRIVATE-TOKEN <t>}
     :codeberg CODEBERG_TOKEN            -> {Authorization \"token <t>\"}
     :clojars  CLOJARS_USERNAME+_PASSWORD-> {Authorization \"Basic <b64>\"}
     :gitea    MAVEN_USERNAME+MAVEN_TOKEN-> {Authorization \"Basic <b64>\"}
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
    :clojars  (let [u (System/getenv "CLOJARS_USERNAME")
                    p (System/getenv "CLOJARS_PASSWORD")]
                (when (and u p)
                  {"Authorization"
                   (str "Basic "
                        (.encodeToString (java.util.Base64/getEncoder)
                                         (.getBytes (str u ":" p))))}))
    :gitea    (let [u (System/getenv "MAVEN_USERNAME")
                    p (System/getenv "MAVEN_TOKEN")]
                (when (and u p)
                  {"Authorization"
                   (str "Basic "
                        (.encodeToString (java.util.Base64/getEncoder)
                                         (.getBytes (str u ":" p))))}))
    :maven    (when-let [t (System/getenv "MAVEN_AUTH")]
                {"Authorization" t})
    {}))

(def github-url "https://github.com/%s/%s")

(defn gitea-registry-url
  "Base URL of the private Gitea Maven registry, read from MAVEN_URL, or nil.
   nil disables Gitea as a resolution source."
  []
  (System/getenv "MAVEN_URL"))
