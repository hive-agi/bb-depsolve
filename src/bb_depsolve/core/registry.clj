(ns bb-depsolve.core.registry
  "Discovery of the private Maven registry a workspace resolves against.

   Collect  — read the workspace's dep files and Maven's settings.xml.
   Promote  — parse them into registry values.
   Boundary — `discover` hands the winning registry to bb-depsolve.core.auth.

   Contract: a registry is identified by the repository id shared between a
   dep file's `:mvn/repos` entry and a `settings.xml` `<server>`. The url comes
   from the former, the credentials from the latter. Environment variables
   (MAVEN_URL / MAVEN_USERNAME / MAVEN_TOKEN) are resolved by
   bb-depsolve.core.auth and take precedence over anything discovered here."
  (:require [babashka.fs :as fs]
            [clojure.data.xml :as xml]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [malli.core :as m]
            [bb-depsolve.schema.api]
            [bb-depsolve.core.discovery :as discovery]))

;; ---------------------------------------------------------------- Calculation

(def public-repo-hosts
  "Hosts whose artifacts need no private credentials."
  #{"repo1.maven.org" "repo.maven.apache.org" "central.sonatype.com"
    "oss.sonatype.org" "s01.oss.sonatype.org" "repo.clojars.org" "clojars.org"})

(defn public-repo?
  "True when URL is a well-known public Maven host. Total: true for nil/blank,
   so an unusable entry is never mistaken for a private registry."
  [url]
  (if (str/blank? url)
    true
    (boolean (some #(str/includes? url %) public-repo-hosts))))

(defn repos-in
  "Every `:mvn/repos` entry declared by one parsed dep-file map, top level and
   under any alias. Returns [{:id string :url string} ...]. Total: [] on nil."
  [dep-edn]
  (when (map? dep-edn)
    (into []
          (comp (mapcat #(seq (:mvn/repos %)))
                (keep (fn [[id {:keys [url]}]]
                        (when (and id url)
                          {:id (name id) :url url}))))
          (cons dep-edn (vals (:aliases dep-edn))))))

(defn private-repos
  "Distinct non-public repositories declared across DEP-EDNS, in declaration
   order. Returns [{:id :url} ...]."
  [dep-edns]
  (into []
        (comp (mapcat repos-in)
              (remove #(public-repo? (:url %)))
              (distinct))
        dep-edns))

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

(m/=> public-repo? [:=> [:cat [:maybe :string]] :boolean])

(m/=> repos-in
      [:=> [:cat [:maybe :any]] [:maybe [:vector :bb-depsolve/mvn-repo]]])

(m/=> private-repos
      [:=> [:cat [:sequential :any]] [:vector :bb-depsolve/mvn-repo]])

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
                (try (edn/read-string (slurp path))
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
