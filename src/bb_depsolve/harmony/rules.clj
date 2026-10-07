(ns bb-depsolve.harmony.rules
  "Ordered rule chain deciding the verdict on one harmony check."
  (:require [malli.core :as m]))

(def verdicts
  "Every verdict `classify` can return."
  #{:unverified :agreed :broken})

(def default-harmony-rules
  "Ordered rule chain deciding a check's verdict. The first rule whose :when
   holds decides; :verdict is a member of `verdicts` or a fn of the context.

   Context keys: :verified? :aot-jars :finding-count

   Every :when is TOTAL — it must not throw on any context, in any order.

   Extend by prepending rules — chain entries are never edited in place.

   The unverified rule comes FIRST on purpose: a check that could not read the
   classpath has a finding-count of zero, which is indistinguishable from
   agreement by any later rule. Ordering is what keeps a blind read from
   reading as a clean one."
  [{:name :unread-inputs-decide-nothing
    :when #(not (:verified? %))
    :verdict :unverified}
   {:name :any-finding-is-a-break
    :when #(pos? (long (:finding-count % 0)))
    :verdict :broken}
   {:name :everything-that-was-read-agrees
    :when (constantly true)
    :verdict :agreed}])

(defn matching-rule
  "The first rule in RULES whose :when holds for CTX, or nil.

   Predicates are applied one at a time, in order — a later rule's predicate
   never runs once an earlier one has matched."
  [rules ctx]
  (some (fn [rule] (when ((:when rule) ctx) rule)) rules))

(defn classify
  "The verdict for CTX under RULES, defaulting to `default-harmony-rules`.

   Returns a member of `verdicts`, or nil when no rule matches."
  ([ctx] (classify default-harmony-rules ctx))
  ([rules ctx]
   (when-let [{:keys [verdict]} (matching-rule rules ctx)]
     (if (fn? verdict) (verdict ctx) verdict))))

(m/=> classify
      [:function
       [:=> [:cat :harmony/verdict-ctx] [:maybe :harmony/verdict]]
       [:=> [:cat [:sequential :map] :harmony/verdict-ctx] [:maybe :harmony/verdict]]])
