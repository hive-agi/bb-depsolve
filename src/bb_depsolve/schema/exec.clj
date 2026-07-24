(ns bb-depsolve.schema.exec
  "Value objects a cascade run emits. Self-registering."
  (:require [hive-schemas.schema :as hs]))

(def schemas
  {:bb-depsolve/exec-status [:enum :released :sync-failed :release-failed]

   :bb-depsolve/run-status [:enum :running :complete :aborted]

   :bb-depsolve/step-outcome
   [:map
    [:project :bb-depsolve/project]
    [:status :bb-depsolve/exec-status]
    [:pin-updates {:optional true} [:vector :bb-depsolve/pin-update]]
    [:version {:optional true} [:maybe :bb-depsolve/version-string]]
    [:tag {:optional true} [:maybe :bb-depsolve/semver-tag]]
    [:error {:optional true} :any]]

   :bb-depsolve/exec-wave
   [:map
    [:index :int]
    [:steps [:vector :bb-depsolve/step-outcome]]
    [:await {:optional true} [:maybe :map]]]

   :bb-depsolve/exec-run
   [:map
    [:status :bb-depsolve/run-status]
    [:waves [:vector :bb-depsolve/exec-wave]]
    [:released [:map-of :bb-depsolve/project :bb-depsolve/version-string]]]})

(defonce ^:private registered?
  (delay (hs/register-all! schemas)))

(defn register!
  "Idempotently register the exec value objects. Returns the keys."
  []
  @registered?)

(register!)
