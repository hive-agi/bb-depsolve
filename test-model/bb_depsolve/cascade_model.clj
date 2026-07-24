(ns bb-depsolve.cascade-model
  "recife (TLA+/TLC) model of the release cascade: a DAG of libs where a lib
   may only be released once every dependency it re-pins has already been
   released and published. Mirrors bb-depsolve.exec/run-plan! — release a
   step, await its artifact, then let consumers re-pin.

   Invariants:
     deps-released-first — a lib is never released before a dependency it
                           re-pins, which is the ordering the plan encodes
     released-once       — no lib is released twice within one cascade
     published<=released — nothing is published that was not released
   Liveness:
     eventually-all-released — the cascade drains the whole closure

   JVM-only rung (recife/TLC): clojure -M:test-model:cascade-model"
  (:require [recife.core :as r]
            [recife.helpers :as rh]
            [hive-recife.core :as hr]))

;; weave <- system <- carto: a chain, so ordering has something to violate.
(def deps
  "lib -> the libs it re-pins, which must be released before it."
  {:weave #{}
   :system #{:weave}
   :carto #{:system :weave}})

(def libs (set (keys deps)))

(def global
  {::released #{}
   ::published #{}
   ::release-count {:weave 0 :system 0 :carto 0}})

(defn- ready?
  "A lib may be released when every dependency it re-pins is PUBLISHED —
   published, not merely released, because a consumer cannot pin a
   coordinate that does not yet resolve."
  [db lib]
  (and (not (contains? (::released db) lib))
       (every? (::published db) (get deps lib))))

(r/defproc releaser
  {:procs #{:release}
   :local {:pc ::step}}
  {[::step {:lib libs}]
   (fn [{:keys [lib] :as db}]
     (if (ready? db lib)
       (-> db
           (update ::released conj lib)
           (update-in [::release-count lib] inc))
       db))})

(r/defproc publisher
  {:procs #{:publish}
   :local {:pc ::await}}
  {[::await {:lib libs}]
   (fn [{:keys [lib] :as db}]
     (if (and (contains? (::released db) lib)
              (not (contains? (::published db) lib)))
       (update db ::published conj lib)
       db))})

(def ^:private visit-order [:weave :system :carto])

;; Deterministic scheduler: release then publish each lib in dependency order,
;; so convergence needs no fairness assumption beyond this process.
(r/defproc ^:fair fair-cascade
  {:procs #{:fair}
   :local {:pc ::wave}}
  {[::wave {}]
   (fn [db]
     (if-let [lib (first (remove (::published db) visit-order))]
       (if (ready? db lib)
         (-> db
             (update ::released conj lib)
             (update-in [::release-count lib] inc)
             (update ::published conj lib))
         (if (contains? (::released db) lib)
           (update db ::published conj lib)
           db))
       db))})

(rh/definvariant deps-released-first [db]
  (every? (fn [lib]
            (or (not (contains? (::released db) lib))
                (every? (::published db) (get deps lib))))
          libs))

(rh/definvariant released-once [db]
  (every? (fn [lib] (<= (get (::release-count db) lib) 1)) libs))

(rh/definvariant published<=released [db]
  (every? (::released db) (::published db)))

(rh/defproperty eventually-all-released [db]
  (rh/eventually (= (::published db) libs)))

(defn safety-spec []
  {:name       ::cascade-safety
   :init-state global
   :components [releaser publisher
                deps-released-first released-once published<=released]
   :safety     ["deps-released-first" "released-once" "published<=released"]
   :liveness   []})

(defn liveness-spec []
  {:name       ::cascade-liveness
   :init-state global
   :components [fair-cascade eventually-all-released]
   :safety     []
   :liveness   ["eventually-all-released"]})

(defn- run-fn
  "Effect fn for hr/check!: deadlock checking off — the drained cascade
   (every lib published) is intended termination."
  [{:keys [init-state components]}]
  (let [ret (r/run-model init-state (set components) {:no-deadlock true})]
    (if (instance? clojure.lang.IDeref ret) (deref ret) ret)))

(defn -main [& _]
  (let [safety   (hr/check! (safety-spec) run-fn)
        liveness (hr/check! (liveness-spec) run-fn)]
    (println :safety   (:status safety)   (some-> safety :details pr-str))
    (println :liveness (:status liveness) (some-> liveness :details pr-str))
    (System/exit (if (and (= :ok (:status safety))
                          (= :ok (:status liveness)))
                   0 1))))
