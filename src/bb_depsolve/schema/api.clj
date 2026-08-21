(ns bb-depsolve.schema.api
  "Domain model as malli schemas, registered into the hive-spi registry.
   Promote layer: raw parsed maps -> validated domain values. Pure data;
   validation happens at the boundary (bb-depsolve.core.api), never inside the
   Calculation layer (bb-depsolve.version.api)."
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
   :bb-depsolve/registry-source [:enum :tags :registry]
   :bb-depsolve/project       :string
   :bb-depsolve/coord         [:enum :git :mvn]
   :bb-depsolve/pin-scope     [:enum :runtime :alias]
   :bb-depsolve/release-mode  [:enum :pinned :rolling]
   :bb-depsolve/bump-kind     [:enum :patch :minor :major]
   :bb-depsolve/role          [:enum :seed :consumer]
   :bb-depsolve/await-mode    [:enum :wait :skip]

   :bb-depsolve/git-dep
   [:map [:lib :bb-depsolve/lib] [:tag :string] [:sha :string] [:match :string]]

   :bb-depsolve/mvn-dep
   [:map [:lib :bb-depsolve/lib] [:version :string] [:match :string]]

   :bb-depsolve/resolved-lib
   [:and [:map [:tag {:optional true} :string] [:sha {:optional true} :string] [:sha-short {:optional true} :string] [:mvn-version {:optional true} :string]] [:fn #:error{:message "must resolve a git coordinate, a Maven coordinate, or both"} (fn [{:keys [tag sha mvn-version]}] (or (and tag sha) mvn-version))]]

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

   :bb-depsolve/repo-id :string

   ;; A repository as a dep file declares it: id and url, no credentials yet.
   :bb-depsolve/mvn-repo
   [:map [:id :bb-depsolve/repo-id] [:url :string]]

   ;; The same repository once settings.xml has supplied credentials for its
   ;; id. Both halves are required — a registry we cannot authenticate to is
   ;; not a resolution source, so it is never represented as one.
   :bb-depsolve/private-registry
   [:map
    [:id :bb-depsolve/repo-id]
    [:url :string]
    [:username :string]
    [:password :string]]

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
    [:missing [:set :bb-depsolve/lib]]]

   :bb-depsolve/graph-node
   [:map
    [:project :bb-depsolve/project]
    [:lib :bb-depsolve/lib]
    [:dir :string]
    [:release-mode :bb-depsolve/release-mode]
    [:version [:maybe :string]]]

   :bb-depsolve/graph-pin
   [:map
    [:project :bb-depsolve/project]
    [:dep :bb-depsolve/project]
    [:lib :bb-depsolve/lib]
    [:coord :bb-depsolve/coord]
    [:scope :bb-depsolve/pin-scope]
    [:version :string]
    [:path :string]]

   :bb-depsolve/internal-graph
   [:map
    [:nodes [:map-of :bb-depsolve/project :bb-depsolve/graph-node]]
    [:edges [:map-of :bb-depsolve/project [:set :bb-depsolve/project]]]
    [:pins [:map-of [:tuple :bb-depsolve/project :bb-depsolve/project]
            [:vector :bb-depsolve/graph-pin]]]]

   :bb-depsolve/pin-update
   [:map
    [:dep :bb-depsolve/project]
    [:lib :bb-depsolve/lib]
    [:coord :bb-depsolve/coord]
    [:path :string]
    [:from :string]
    [:to [:maybe :string]]]

   :bb-depsolve/cascade-step
   [:map
    [:project :bb-depsolve/project]
    [:lib :bb-depsolve/lib]
    [:dir :string]
    [:role :bb-depsolve/role]
    [:release-mode :bb-depsolve/release-mode]
    [:current-version [:maybe :string]]
    [:bump-kind [:maybe :bb-depsolve/bump-kind]]
    [:next-version [:maybe :string]]
    [:version-drift {:optional true}
     [:map [:declared [:maybe :string]] [:observed :string]]]
    [:pin-updates [:vector :bb-depsolve/pin-update]]]

   :bb-depsolve/await-policy
   [:map
    [:mode :bb-depsolve/await-mode]
    [:timeout-ms :int]]

   :bb-depsolve/await
   [:map
    [:mode :bb-depsolve/await-mode]
    [:timeout-ms :int]
    [:libs [:vector [:map
                     [:lib :bb-depsolve/lib]
                     [:newer-than [:maybe :string]]
                     [:expect [:maybe :string]]]]]]

   :bb-depsolve/cascade-wave
   [:map
    [:index :int]
    [:steps [:vector :bb-depsolve/cascade-step]]
    [:await :bb-depsolve/await]]

   :bb-depsolve/cascade-plan
   [:map
    [:seeds [:set :bb-depsolve/project]]
    [:unknown-seeds [:set :bb-depsolve/project]]
    [:policy [:map
              [:requested-bump :bb-depsolve/bump-kind]
              [:await :bb-depsolve/await-policy]]]
    [:waves [:vector :bb-depsolve/cascade-wave]]
    [:cycles [:vector [:set :bb-depsolve/project]]]
    [:excluded [:vector [:map
                         [:project :bb-depsolve/project]
                         [:reason [:enum :cycle-member :blocked-by-cycle]]]]]]

   :bb-depsolve/sync-outcome
   [:map
    [:project :bb-depsolve/project]
    [:paths [:set :string]]
    [:applied [:vector :bb-depsolve/pin-update]]
    [:skipped [:vector :bb-depsolve/pin-update]]]

   :bb-depsolve/release-outcome
   [:map
    [:project :bb-depsolve/project]
    [:release-mode :bb-depsolve/release-mode]
    [:version :bb-depsolve/version-string]
    [:tag [:maybe :bb-depsolve/semver-tag]]]})

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