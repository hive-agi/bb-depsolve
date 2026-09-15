(ns bb-depsolve.core.upgrade.guard
  "What `upgrade` may move, decided per consuming dep file. Pure.

   pinned-dep   {:path :project :lib :version :consumer-repos}
   upgrade-row  {:path :project :lib :old-version :new-version}
   held-row     upgrade-row plus
                  :reason    :major | :downgrade | :unreachable
                  :registry  repo id (only for :unreachable)"
  (:require [bb-depsolve.version.api :as v]
            [bb-depsolve.version.repos :as repos]
            [bb-depsolve.version.semver :as semver]
            [clojure.string :as str]))

(def default-org
  "The internal org whose io.github.<org>/* libs `sync` owns."
  "hive-agi")

;; =============================================================================
;; Lib selection
;; =============================================================================

(defn parse-libs
  "The lib symbols a comma-separated CSV names, or nil when CSV is nil or
   blank. Entries are trimmed; a bare `foo` names `foo/foo`."
  [csv]
  (when-not (str/blank? (some-> csv str))
    (let [libs (->> (str/split (str csv) #",")
                    (map str/trim)
                    (remove str/blank?)
                    (map #(symbol (if (str/includes? % "/") % (str % "/" %))))
                    (into #{}))]
      (not-empty libs))))

(defn internal-lib?
  "True when LIB is in ORG's io.github group (default `default-org`)."
  [org lib]
  (v/lib-matches-org? (or org default-org) lib))

(defn partition-libs
  "Split LIBS by OPTS :org, :only and :exclude (csv strings).
   => {:kept [lib] :internal [lib] :excluded [lib]}, each sorted. An internal
   lib is never kept, whatever :only names."
  [libs {:keys [org only exclude]}]
  (let [only-set (parse-libs only)
        excluded (or (parse-libs exclude) #{})
        bucket (fn [lib]
                 (cond
                   (internal-lib? org lib) :internal
                   (and only-set (not (contains? only-set lib))) :excluded
                   (contains? excluded lib) :excluded
                   :else :kept))
        groups (group-by bucket (sort-by str libs))]
    {:kept     (vec (:kept groups))
     :internal (vec (:internal groups))
     :excluded (vec (:excluded groups))}))

(defn apply-mode
  "How --apply chooses upgrades: :explicit when OPTS carry --all or a
   non-blank --only, :interactive otherwise."
  [{:keys [all only]}]
  (if (or all (parse-libs only)) :explicit :interactive))

;; =============================================================================
;; Version policy
;; =============================================================================

(defn held-jump?
  "True when OLD -> NEW is an upgrade that crosses a major version, or a minor
   version while the major is 0 (0.2.x -> 0.4.x). Versions with fewer than two
   numeric segments (calendar builds such as 20240303) never qualify. Total:
   false for nil or unparseable input."
  [old new]
  (let [o (semver/parse-version-segments old)
        n (semver/parse-version-segments new)]
    (boolean
     (and (>= (count o) 2) (>= (count n) 2)
          (semver/version-newer? old new)
          (let [[omaj omin] o
                [nmaj nmin] n]
            (or (> nmaj omaj)
                (and (zero? omaj) (= nmaj omaj) (> nmin omin))))))))

(defn consumer-candidate
  "ROWS ([registry-version]) as one consumer declaring CONSUMER-REPOS sees
   them, through the same projection `sync` pins with (repos/project-lib).
   => {:version newest-reachable-or-nil :unreachable [registry-version]}, where
   :unreachable lists newer versions on registries the consumer does not
   declare."
  [rows consumer-repos]
  (let [rows (vec rows)
        projected (when (seq rows)
                    (repos/project-lib {:mvn-by-registry rows
                                        :mvn-version (:version (repos/newest rows))}
                                       (vec consumer-repos)))]
    (if projected
      {:version (:mvn-version projected)
       :unreachable (vec (:mvn-unreachable projected))}
      {:version nil
       :unreachable (vec (sort-by :version semver/version-compare
                                  (remove #(repos/reachable? consumer-repos %) rows)))})))

(defn decide
  "The verdict on one pinned DEP ({:path :project :lib :version}) given the
   consumer's CANDIDATE (see `consumer-candidate`). => nil when nothing moves,
   an upgrade-row, or a held-row:
     :downgrade    the newest reachable version is OLDER than the pin
     :major        a `held-jump?` and OPTS :allow-major is not set
     :unreachable  a newer version exists only on a registry the consumer
                   does not declare"
  [{:keys [version] :as dep} {cand :version unreachable :unreachable} {:keys [allow-major]}]
  (let [row (-> dep (dissoc :version :consumer-repos) (assoc :old-version version))
        beyond (repos/newest unreachable)]
    (cond
      (and cand (v/downgrade-change? {:coord :mvn :old-version version :new-version cand}))
      (assoc row :new-version cand :reason :downgrade)

      (and cand (semver/version-newer? version cand))
      (cond-> (assoc row :new-version cand)
        (and (held-jump? version cand) (not allow-major)) (assoc :reason :major))

      (and beyond (semver/version-newer? version (:version beyond)))
      (assoc row :new-version (:version beyond) :reason :unreachable :registry (:id beyond)))))

(defn plan
  "Split DEPS ([pinned-dep]) against LATEST ({lib [registry-version]}) into
   {:upgrades [upgrade-row] :held [held-row]}, both distinct and in DEPS
   order. A dep whose lib has no rows, or with nothing to move, is in neither.
   OPTS: :allow-major."
  [deps latest opts]
  (let [rows (->> deps
                  (keep (fn [{:keys [lib consumer-repos] :as dep}]
                          (when-let [lib-rows (seq (get latest lib))]
                            (decide dep (consumer-candidate lib-rows consumer-repos) opts))))
                  (distinct))]
    {:upgrades (vec (remove :reason rows))
     :held     (vec (filter :reason rows))}))
