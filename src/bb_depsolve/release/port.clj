(ns bb-depsolve.release.port
  "Release effects and registry observations.

   sync-outcome    {:project :paths #{path} :applied [..] :skipped [..]}
   release-outcome {:project :release-mode :version :tag}"
  (:require [bb-depsolve.version.api :as v]
            [hive-dsl.result :as r]
            [bb-depsolve.version.repos :as repos]))

(defprotocol IArtifactRegistry
  "Read-only registry observations. Each method returns a Result."
  (published? [this lib version]
    "=> Result of boolean: exactly VERSION of LIB resolves somewhere.")
  (latest-version [this lib]
    "=> Result of the highest resolvable version of LIB, or nil.")
  (published-versions [this lib]
    "=> Result of [{:id :url :public? :kind :version}]: every version of LIB
     each source lists, one entry per (source, version). :kind is :git for a
     tag, :mvn for a Maven artifact on the registry :id names, :any for a
     source that serves both. :public? and :url say which consumers can
     reach it (bb-depsolve.version.repos/reachable?)."))

(defprotocol IReleasePort
  "Effects of one cascade step. Each method returns a Result."
  (sync-pins! [this step]
    "Rewrite STEP's :pin-updates; an update with a nil :to is skipped.
     => Result of sync-outcome.")
  (release! [this step]
    "Bump, commit, tag and push under :pinned; commit and push under
     :rolling, where the push mints the version.
     => Result of release-outcome."))

(defn satisfies-consumer?
  "True when some entry of PUBLISHED at VERSION is fetchable by CONSUMER
   {:coord :repos}: a :git consumer needs a tag, a :mvn consumer needs a
   Maven artifact on a registry its declared repos reach. Pure."
  [published version {:keys [coord repos]}]
  (boolean
   (some (fn [{:keys [kind] :as entry}]
           (and (= version (:version entry))
                (or (= :any kind)
                    (and (= :git coord) (= :git kind))
                    (and (= :mvn coord) (= :mvn kind)
                         (repos/reachable? (or repos []) entry)))))
         published)))

(defn await-satisfied?
  "=> Result of boolean: REGISTRY meets the await entry
   {:lib :newer-than :expect :reach}. With an :expect, that exact version
   must resolve; without one, any version newer than :newer-than does.

   With a :reach (how the plan's later consumers fetch the lib, as
   [{:coord :repos}]), the version must be fetchable by EVERY consumer:
   published anywhere is not published for a public consumer when only the
   private registry has it. Without a :reach, any source counts."
  [registry {:keys [lib newer-than expect reach]}]
  (if (empty? reach)
    (if expect
      (published? registry lib expect)
      (r/let-ok [latest (latest-version registry lib)]
        (r/ok (boolean (and latest
                            (or (nil? newer-than)
                                (v/version-newer? newer-than latest)))))))
    (r/let-ok [published (published-versions registry lib)]
      (let [candidates (if expect
                         #{(v/tag->mvn-version expect)}
                         (into #{}
                               (comp (map :version)
                                     (filter #(or (nil? newer-than) (v/version-newer? newer-than %))))
                               published))]
        (r/ok (boolean (some (fn [version]
                               (every? #(satisfies-consumer? published version %) reach))
                             candidates)))))))

;; =============================================================================
;; In-memory adapter
;; =============================================================================

(defn- default-mint
  "Version a :rolling step lands on: its current version, patch-bumped."
  [{:keys [current-version]}]
  (or (some-> current-version v/parse-semver v/bump-patch v/semver->version)
      "0.0.1"))

(defn- poll!
  "Advance LIB's pending publication one poll, promoting it into the registry
   when the delay elapses. => the new state."
  [state lib]
  (swap! state
         (fn [s]
           (if-let [{:keys [version polls-left]} (get-in s [:pending lib])]
             (if (<= (long polls-left) 1)
               (-> s
                   (update-in [:registry lib] (fnil conj #{}) version)
                   (update :pending dissoc lib))
               (update-in s [:pending lib :polls-left] dec))
             s))))

(defrecord MemoryPort [state opts]
  IReleasePort
  (sync-pins! [_ step]
    (let [{:keys [project pin-updates]} step
          {applied true skipped false} (group-by (comp some? :to) pin-updates)]
      (swap! state
             (fn [s]
               (-> (reduce (fn [acc {:keys [path lib to]}]
                             (assoc-in acc [:pins path lib] to))
                           s applied)
                   (update :log conj [:sync-pins project (mapv :dep applied)]))))
      (r/ok {:project project
             :paths (into (sorted-set) (map :path) applied)
             :applied (vec applied)
             :skipped (vec skipped)})))

  (release! [_ step]
    (let [{:keys [project lib release-mode next-version]} step]
      (if (contains? (set (:fail opts)) project)
        (r/err {:kind :release-failed :project project})
        (let [version (or next-version ((:mint opts default-mint) step))
              delay-polls (long (:publish-after opts 0))]
          (swap! state
                 (fn [s]
                   (cond-> (-> s
                               (assoc-in [:released project] version)
                               (update :log conj [:release project version]))
                     (zero? delay-polls)
                     (update-in [:registry lib] (fnil conj #{}) version)

                     (pos? delay-polls)
                     (assoc-in [:pending lib] {:version version
                                               :polls-left delay-polls}))))
          (r/ok {:project project
                 :release-mode release-mode
                 :version version
                 :tag (when (= :pinned release-mode) (str "v" version))})))))

  IArtifactRegistry
  (published? [_ lib version]
    (let [s (poll! state lib)]
      (r/ok (contains? (get-in s [:registry lib] #{})
                       (v/tag->mvn-version version)))))

  (latest-version [_ lib]
    (let [s (poll! state lib)]
      (r/ok (last (sort v/version-compare (get-in s [:registry lib] #{}))))))

  (published-versions [_ lib]
    (let [s (poll! state lib)]
      (r/ok (mapv (fn [version] {:id "memory" :public? true :kind :any :version version})
                  (sort v/version-compare (get-in s [:registry lib] #{})))))))

(defn memory-port
  "In-memory IReleasePort + IArtifactRegistry.

   OPTS:
     :registry       {lib #{version}} already resolvable
     :publish-after  polls a released artifact takes to resolve (default 0)
     :mint           (fn [step] version) for :rolling releases
     :fail           #{project} whose release! errors

   State is readable at (:state port)."
  ([] (memory-port {}))
  ([opts]
   (->MemoryPort (atom {:registry (or (:registry opts) {})
                        :pending {}
                        :pins {}
                        :released {}
                        :log []})
                 opts)))