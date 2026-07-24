(ns bb-depsolve.core.fetch
  "Gated HTTP retrieval of POM and deps.edn artifacts."
  (:require [babashka.http-client :as http]
            [clojure.string :as str]
            [hive-dsl.gate :as gate]
            [hive-dsl.result :as r]
            [bb-depsolve.version :as v]
            [bb-depsolve.ui :as ui]
            [bb-depsolve.schema :as sch]
            [bb-depsolve.core.auth :as auth]))

(def ^:private http-gate (gate/gate {:permits 5 :timeout-ms 30000}))

(defn- fetch-pom-xml
  "Fetch POM XML from a URL. Returns Result<string>.
   Selects auth headers by URL prefix (clojars vs maven)."
  [url]
  (r/try-effect*
   :io/fetch-pom
   (let [target (cond
                  (str/includes? url "clojars.org")    :clojars
                  (str/includes? url "maven.org")      :maven
                  (str/includes? url "repo1.maven")    :maven
                  :else                                 :none)
         headers (auth/auth-headers target)
         resp (gate/gate-run http-gate
                (fn [] (http/get url (merge {:throw false}
                                            (when (seq headers) {:headers headers})))))]
     (if (= 200 (:status resp))
       (:body resp)
       (throw (ex-info "POM not found" {:url url}))))))

(defn- warn-unresolved-coord
  "I/O boundary warn-fn for coordinates dropped because they still contain
   `${...}` property placeholders. Emits a single yellow warning line.
   Keeps the Calculation layer (version.clj) free of logging deps."
  [parent-coord coord]
  (println (ui/c :yellow
              (format "[bb-depsolve] WARN: dropping unresolved Maven property coord %s (from %s)"
                      (pr-str coord)
                      (pr-str parent-coord)))))

(defn fetch-pom-deps
  "Fetch and parse POM for a Maven artifact. Tries Clojars then Maven Central.
   Filters out coords whose key fields still contain `${...}` placeholders
   AFTER POM parsing (see bb-depsolve.version/filter-resolved-coords). Dropped
   coords are logged via `warn-unresolved-coord` (not silently swallowed).
   Returns Result<[{:lib :version}]>, schema-validated at this boundary
   (fail-loud)."
  [group-id artifact-id version]
  (let [[clojars-url maven-url] (v/pom-urls group-id artifact-id version)
        pom-xml (let [r1 (fetch-pom-xml clojars-url)]
                  (if (r/ok? r1) r1 (fetch-pom-xml maven-url)))
        parent  {:group group-id :artifact artifact-id :version version}
        warn-fn (partial warn-unresolved-coord parent)
        filter-step (fn [coords] (v/filter-resolved-coords coords warn-fn))
        validate-step (fn [coords] (sch/validate! :bb-depsolve/pom-deps coords))]
    (r/ok-> pom-xml
            v/parse-pom-deps-raw
            filter-step
            validate-step)))

(defn fetch-git-deps-edn
  "Fetch raw deps.edn content from a forge. Returns Result<string>.
   Uses bb-depsolve.version/forge-raw-url to pick the per-forge URL shape and
   bb-depsolve.core/auth-headers to attach private-registry creds."
  ([org repo tag]
   (fetch-git-deps-edn :github org repo tag))
  ([forge org repo tag]
   (r/try-effect*
    :io/fetch-git-deps
    (let [url (or (v/forge-raw-url forge org repo tag "deps.edn")
                  (throw (ex-info "Unsupported forge" {:forge forge})))
          headers (auth/auth-headers forge)
          resp (gate/gate-run http-gate
                 (fn [] (http/get url (merge {:throw false}
                                             (when (seq headers) {:headers headers})))))]
      (if (= 200 (:status resp))
        (:body resp)
        (throw (ex-info "deps.edn not found"
                        {:forge forge :org org :repo repo :tag tag})))))))

(defn fetch-git-dep-coords
  "Fetch deps.edn from GitHub raw content for a git dep.
   Returns Result<[{:lib :version :type}]>."
  [org repo tag]
  (r/ok-> (fetch-git-deps-edn org repo tag)
          v/deps-edn->dep-coords))
