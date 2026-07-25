(ns bb-depsolve.layer.rules
  "Ordered rule chain classifying one dependency edge against a layer order.")

(def verdicts
  "Every verdict `classify` can return."
  #{:unranked :waived :ok :sideways :violation})

(def default-layer-rules
  "Ordered rule chain deciding an edge's verdict. The first rule whose :when
   holds decides; :verdict is a member of `verdicts` or a fn of the context.

   Context keys: :from :to :from-level :to-level :to-terminal? :waived?

   Every :when is TOTAL — it must not throw on any context, in any order.

   Extend by prepending rules — chain entries are never edited in place."
  [{:name :unranked-project-is-exempt
    :when #(or (nil? (:from-level %)) (nil? (:to-level %)))
    :verdict :unranked}
   {:name :waived-edge-is-allowed
    :when :waived?
    :verdict :waived}
   {:name :terminal-layer-may-not-be-depended-on
    :when :to-terminal?
    :verdict :violation}
   {:name :downward-edge-is-the-point
    :when #(let [{:keys [from-level to-level]} %]
             (boolean (and from-level to-level (> (long from-level) (long to-level)))))
    :verdict :ok}
   {:name :sideways-edge-is-allowed
    :when #(let [{:keys [from-level to-level]} %]
             (boolean (and from-level to-level (= from-level to-level))))
    :verdict :sideways}
   {:name :upward-edge-is-a-violation
    :when (constantly true)
    :verdict :violation}])

(defn matching-rule
  "The first rule in RULES whose :when holds for CTX, or nil.

   Predicates are applied one at a time, in order — a later rule's predicate
   never runs once an earlier one has matched."
  [rules ctx]
  (some (fn [rule] (when ((:when rule) ctx) rule)) rules))

(defn classify
  "The verdict for CTX under RULES, defaulting to `default-layer-rules`.

   Returns a member of `verdicts`, or nil when no rule matches."
  ([ctx] (classify default-layer-rules ctx))
  ([rules ctx]
   (when-let [{:keys [verdict]} (matching-rule rules ctx)]
     (if (fn? verdict) (verdict ctx) verdict))))
