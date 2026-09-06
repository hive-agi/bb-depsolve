(ns bb-depsolve.core.registry
  "Discovery of the private Maven registry a workspace resolves against.

   Collect  — read the workspace's dep files and Maven's settings.xml.
   Promote  — parse them into registry values.
   Boundary — `discover` hands the winning registry to bb-depsolve.core.auth.

   Contract: a registry is identified by the repository id shared between a
   dep file's `:mvn/repos` entry and a `settings.xml` `<server>`. The url comes
   from the former, the credentials from the latter. Environment variables
   (MAVEN_URL / MAVEN_USERNAME / MAVEN_TOKEN) are resolved by
   bb-depsolve.core.auth and take precedence over anything discovered here.

   The pure half (which repositories a dep file declares, and which of them
   are public) lives in bb-depsolve.version.repos and is re-exported here."
  (:require [babashka.fs :as fs]
            [clojure.data.xml :as xml]
            [clojure.string :as str]
            [malli.core :as m]
            [bb-depsolve.schema.api]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.version.repos :as repos]))

;; ---------------------------------------------------------------- Calculation

(def public-repo-hosts repos/public-repo-hosts)
(def public-repo? repos/public-repo?)
(def repos-in repos/repos-in)
(def private-repos repos/private-repos)

(defn parse-settings-servers
  "Credentials from a Maven settings.xml body, as {id {:username :password}}.
   Total: {} on nil/unparseable."
  [xml-string]
  (try
    (let [tag= (fn [el t] (and (map? el) (keyword? (:tag el)) (= (name (:tag el)) t)))
          find1 (fn [parent t] (first (filter #(tag= % t) (:content parent))))
          text (fn [el] (when el (str/trim (apply str (filter string? (:content el))))))
          servers (find1 (xml/parse-str xml-string) "servers")]
      (into {}
            (keep (fn [server]
                    (let [id (text (find1 server "id"))
                          username (text (find1 server "username"))
                          password (text (find1 server "password"))]
                      (when (and (not (str/blank? id))
                                 (not (str/blank? username))
                                 (not (str/blank? password)))
                        [id {:username username :password password}]))))
            (filter #(tag= % "server") (:content servers))))
    (catch Exception _ {})))

(defn with-credentials
  "Join REPOS to SERVERS by repository id, keeping only repositories whose
   credentials are known. Returns [{:id :url :username :password} ...]."
  [repos servers]
  (into []
        (keep (fn [{:keys [id] :as repo}]
                (when-let [creds (get servers id)]
                  (merge repo creds))))
        repos))

(m/=> parse-settings-servers
      [:=> [:cat [:maybe :string]]
       [:map-of :bb-depsolve/repo-id
        [:map [:username :string] [:password :string]]]])

(m/=> with-credentials
      [:=> [:cat [:sequential :bb-depsolve/mvn-repo]
            [:map-of :bb-depsolve/repo-id
             [:map [:username :string] [:password :string]]]]
       [:vector :bb-depsolve/private-registry]])

;; ------------------------------------------------------------------- Boundary

(defn settings-file
  "Path to Maven's settings.xml. M2_SETTINGS overrides ~/.m2/settings.xml."
  []
  (or (System/getenv "M2_SETTINGS")
      (str (fs/path (System/getProperty "user.home") ".m2" "settings.xml"))))

(defn read-servers
  "Credentials declared in Maven's settings.xml. {} when it is absent.
   The path is a collaborator: callers may hand one in, and only the 0-arity
   consults the environment."
  ([] (read-servers (settings-file)))
  ([path]
   (if (and path (fs/exists? path))
     (parse-settings-servers (slurp path))
     {})))

(defn read-dep-edns
  "Parse every dep file in the workspace described by OPTS. Unreadable files
   contribute nothing rather than failing the scan."
  [opts]
  (into []
        (keep (fn [{:keys [path]}]
                (try (repos/read-dep-content (slurp path))
                     (catch Exception _ nil))))
        (discovery/find-dep-files opts)))

(defn discover
  "The private Maven registry this workspace resolves against, or nil.

   A registry qualifies when a dep file declares it under `:mvn/repos` AND
   settings.xml carries credentials under the same repository id. The first
   such registry wins; nil means the workspace declares none we can
   authenticate to, and private-registry resolution stays disabled.

   OPTS is the dep-file scan (`:root` / `:skip-dirs` / `:depth`) plus an
   optional `:settings` path, which lets a caller supply the credentials file
   instead of the environment."
  [{:keys [settings] :as opts}]
  (first (with-credentials (private-repos (read-dep-edns opts))
                           (if settings
                             (read-servers settings)
                             (read-servers)))))
