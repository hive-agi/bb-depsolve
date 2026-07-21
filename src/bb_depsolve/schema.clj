(ns bb-depsolve.schema
  "Domain model as malli schemas, registered into the hive-spi registry.
   Promote layer: raw parsed maps -> validated domain values. Pure data;
   validation happens at the boundary (bb-depsolve.core), never inside the
   Calculation layer (bb-depsolve.version)."
  (:require [hive-schemas.schema :as hs]
            [malli.registry :as mr]))

(def semver-tag-re #"^v\d+\.\d+\.\d+$")
(def version-string-re #"^\d+\.\d+\.\d+$")
(def sha-re #"^[0-9a-f]{7,40}$")

(def schemas
  "Domain schemas. Keys are the ubiquitous language of bb-depsolve."
  {:bb-depsolve/semver-triple [:tuple :int :int :int]
   :bb-depsolve/semver-tag    [:re semver-tag-re]
   :bb-depsolve/version-string [:re version-string-re]
   :bb-depsolve/sha           [:re sha-re]
   :bb-depsolve/lib           :symbol

   :bb-depsolve/git-dep
   [:map [:lib :bb-depsolve/lib] [:tag :string] [:sha :string] [:match :string]]

   :bb-depsolve/mvn-dep
   [:map [:lib :bb-depsolve/lib] [:version :string] [:match :string]]

   :bb-depsolve/resolved-lib
   [:map [:tag :string] [:sha :string] [:sha-short {:optional true} :string]]

   :bb-depsolve/resolved
   [:map-of :bb-depsolve/lib :bb-depsolve/resolved-lib]

   :bb-depsolve/sync-change
   [:multi {:dispatch :coord}
    [:git [:map
           [:lib :bb-depsolve/lib] [:coord [:= :git]]
           [:old-tag :string] [:old-sha :string]
           [:new-tag :string] [:new-sha :string]]]
    [:mvn [:map
           [:lib :bb-depsolve/lib] [:coord [:= :mvn]]
           [:old-version :string] [:new-version :string]]]]

   :bb-depsolve/sync-changes
   [:vector :bb-depsolve/sync-change]

   :bb-depsolve/pom-dep
   [:map [:lib :bb-depsolve/lib] [:version :string]]

   :bb-depsolve/pom-deps
   [:vector :bb-depsolve/pom-dep]

   :bb-depsolve/tree-node
   [:map
    [:lib :bb-depsolve/lib]
    [:version :string]
    [:type :keyword]
    [:cycle? :boolean]
    [:children [:vector [:ref :bb-depsolve/tree-node]]]]

   :bb-depsolve/resolution
   [:map
    [:resolved [:map-of :bb-depsolve/lib
                [:map [:version :string] [:type :keyword] [:depth :int]]]]
    [:conflicts [:map-of :bb-depsolve/lib [:set :string]]]
    [:occurrences [:map-of :bb-depsolve/lib
                   [:vector [:map [:version :string] [:type :keyword] [:depth :int]]]]]
    [:missing [:set :bb-depsolve/lib]]]})

(defonce ^:private registered?
  (delay (hs/register-all! schemas)))

(defn register!
  "Idempotently register all bb-depsolve domain schemas. Returns the keys."
  []
  @registered?)

(mr/set-default-registry! hs/registry)

(register!)

(defn validate!
  "Fail-loud validation at a boundary. Returns value when valid; throws
   ex-info with the malli explain-data otherwise."
  [schema-key value]
  (if (hs/validate schema-key value)
    value
    (throw (ex-info (str "bb-depsolve schema violation: " schema-key)
                    {:schema schema-key
                     :explain (hs/explain schema-key value)}))))

(comment
  (require '[malli.generator :as mg])
  (register!)
  (mg/generate :bb-depsolve/sync-change {:seed 42}))