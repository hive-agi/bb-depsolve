(ns bb-depsolve.port
  "Release effects and registry observations.

   sync-outcome    {:project :paths #{path} :applied [..] :skipped [..]}
   release-outcome {:project :release-mode :version :tag}"
  (:require [bb-depsolve.version :as v]
            [hive-dsl.result :as r]))

(defprotocol IArtifactRegistry
  "Read-only registry observations. Each method returns a Result."
  (published? [this lib version]
    "=> Result of boolean: exactly VERSION of LIB resolves.")
  (latest-version [this lib]
    "=> Result of the highest resolvable version of LIB, or nil."))

(defprotocol IReleasePort
  "Effects of one cascade step. Each method returns a Result."
  (sync-pins! [this step]
    "Rewrite STEP's :pin-updates; an update with a nil :to is skipped.
     => Result of sync-outcome.")
  (release! [this step]
    "Bump, commit, tag and push under :pinned; commit and push under
     :rolling, where the push mints the version.
     => Result of release-outcome."))

(defn await-satisfied?
  "=> Result of boolean: REGISTRY meets the await entry
   {:lib :newer-than :expect}. With an :expect, that exact version must
   resolve; without one, any version newer than :newer-than does."
  [registry {:keys [lib newer-than expect]}]
  (if expect
    (published? registry lib expect)
    (r/let-ok [latest (latest-version registry lib)]
      (r/ok (boolean (and latest
                          (or (nil? newer-than)
                              (v/version-newer? newer-than latest))))))))

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
      (r/ok (last (sort v/version-compare (get-in s [:registry lib] #{})))))))

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