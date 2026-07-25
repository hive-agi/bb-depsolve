(ns bb-depsolve.cascade.bump
  (:require [bb-depsolve.version :as v]))

(declare bump-kinds bump-rank default-bump-rules select-bump-kind strongest-bump bump-fn next-version)

(def bump-kinds
  "Semver segment a release advances. Ordered weakest to strongest."
  [:patch :minor :major])

(def ^:private bump-rank
  (into {nil 0} (map-indexed (fn [i k] [k (inc i)])) bump-kinds))

(def default-bump-rules
  "Ordered rule chain deciding a project's bump kind. The first rule whose
   :when holds decides; :bump is a bump-kind, nil, or a fn of the context.

   Context keys: :project :role :release-mode :requested-bump :upstream-bump
   :wave-index.

   Extend by prepending rules — chain entries are never edited in place."
  [{:name :rolling-carries-no-bump
    :when #(= :rolling (:release-mode %))
    :bump nil}
   {:name :seed-honours-request
    :when #(and (= :seed (:role %))
                (contains? (set bump-kinds) (:requested-bump %)))
    :bump (fn [ctx] (:requested-bump ctx))}
   {:name :consumer-of-major-takes-minor
    :when #(and (= :consumer (:role %)) (= :major (:upstream-bump %)))
    :bump :minor}
   {:name :default-patch
    :when (constantly true)
    :bump :patch}])

(defn select-bump-kind
  "Bump kind decided by the first matching rule in RULES for CTX.
   Returns a bump-kind or nil."
  ([ctx] (select-bump-kind default-bump-rules ctx))
  ([rules ctx]
   (when-let [rule (first (filter #((:when %) ctx) rules))]
     (let [b (:bump rule)]
       (if (fn? b) (b ctx) b)))))

(defn strongest-bump
  "The strongest bump kind among KINDS. Returns nil when all are nil/empty."
  [kinds]
  (->> kinds
       (sort-by bump-rank)
       last))

(defn bump-fn
  [bump-kind]
  (case bump-kind
    :patch v/bump-patch
    :minor v/bump-minor
    :major v/bump-major
    nil))

(defn next-version
  "Version string CURRENT advances to under BUMP-KIND.
   Returns nil when BUMP-KIND is nil or CURRENT is unparseable."
  [current bump-kind]
  (when-let [f (bump-fn bump-kind)]
    (when-let [semver (some-> current v/parse-semver)]
      (v/semver->version (f semver)))))
