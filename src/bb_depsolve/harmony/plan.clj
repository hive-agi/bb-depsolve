(ns bb-depsolve.harmony.plan
  "Pipeline: jar scans plus one Clojure jar's class list -> a harmony report.

   Pure. The scans arrive already read (see `bb-depsolve.harmony.store`), so
   this is the layer a test can drive with literal maps."
  (:require [bb-depsolve.harmony.refs :as refs]
            [bb-depsolve.harmony.rules :as rules]
            [bb-depsolve.harmony.schema]
            [clojure.string :as str]
            [malli.core :as m]))

(def ^:private displayed-missing
  "How many missing classes a finding carries for display. The count is the
   measurement; the names are an example to grep for."
  3)

(defn finding
  "The finding for one SCAN against SUBJECT, or nil when the jar agrees.

   SUBJECT is the artifact under test as {:classes :namespaces} — the class
   names it ships and the namespaces it owns. The scan's demands are narrowed
   to those namespaces first: a jar's constant pool names classes from every
   library it was compiled against, and only the ones belonging to the subject
   say anything about the subject's version."
  [{:keys [jar required self-provided]} {:keys [classes namespaces]}]
  (let [mine (refs/owned-by (or required #{}) namespaces)
        gone (refs/missing mine classes (or self-provided #{}))]
    (when (seq gone)
      {:jar jar
       :missing-count (count gone)
       :missing (vec (take displayed-missing gone))})))

(defn report
  "The harmony report for SCANS against SUBJECT (see `finding`).

   VERIFIED? says whether the inputs were actually read; passing false yields
   an :unverified verdict whatever the scans say, because an unread classpath
   produces no findings and would otherwise read as agreement."
  ([scans subject] (report scans subject true))
  ([scans subject verified?]
   (let [aot (filterv #(and (:aot? %) (not (:clojure-artifact? %))) scans)
         findings (vec (keep #(finding % subject) aot))]
     {:verdict (rules/classify {:verified? (boolean verified?)
                                :aot-jars (count aot)
                                :finding-count (count findings)})
      :findings findings
      :aot-jars (count aot)
      :unreadable (vec (keep #(when (:unreadable %) (:jar %)) scans))})))

(defn blind
  "The report for a check that could not be made, carrying REASON."
  [reason]
  (assoc (report [] {:classes #{} :namespaces #{}} false) :reason reason))

(defn broken?
  "Did REPORT find a real disagreement? False for :unverified — a blind read
   must never be reported as a break, nor as a pass."
  [report]
  (= :broken (:verdict report)))

(defn explain
  "One line naming why REPORT holds a change back."
  [{:keys [findings clojure-version verdict reason]}]
  (if-let [{:keys [jar missing-count missing]} (first findings)]
    (format "AOT mismatch: %s hard-references %d class(es) Clojure %s does not ship (e.g. %s)"
            (last (str/split (str jar) #"/"))
            missing-count
            (or clojure-version "?")
            (first missing))
    (format "%s%s" (name (or verdict :unverified))
            (if reason (str ": " (name reason)) ""))))

(m/=> finding [:=> [:cat :map :harmony/subject] [:maybe :harmony/finding]])
(m/=> broken? [:=> [:cat :map] :boolean])
(m/=> explain [:=> [:cat :map] :string])
