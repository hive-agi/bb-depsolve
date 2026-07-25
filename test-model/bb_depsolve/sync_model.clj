(ns bb-depsolve.sync-model
  "recife (TLA+/TLC) model of the sync operator: a workspace of N libs, each
   step syncs one drifted lib to its resolved version. Mirrors the semantics
   of bb-depsolve.version.api/sync-changes-in-content — drift is ANY difference
   from resolved, and applying a change pins current := resolved.

   Invariants:
     applied-stays-synced  — once a lib is synced it is never re-broken
     emitted<=>drift       — emitted change set is nonempty IFF drift exists
   Liveness:
     eventually-all-synced — the workspace converges to the resolved state

   JVM-only rung (recife/TLC): clojure -M:test-model"
  (:require [recife.core :as r]
            [recife.helpers :as rh]
            [hive-recife.core :as hr]))

(def libs #{:a :b :c})

(def global
  {::current {:a 0 :b 0 :c 0}
   ::target  {:a 2 :b 1 :c 2}
   ::synced  #{}
   ::rota    0})

(r/defproc syncer
  {:procs #{:sync}
   :local {:pc ::step}}
  {[::step {:lib libs}]
   (fn [{:keys [lib] :as db}]
     (if (not= (get (::current db) lib) (get (::target db) lib))
       (-> db
           (assoc-in [::current lib] (get (::target db) lib))
           (update ::synced conj lib))
       db))})

(def ^:private visit-order [:a :b :c])

;; Deterministic round-robin scheduler: ::rota changes every step, so no
;; stuttering trace exists and convergence needs no fairness assumption.
(r/defproc ^:fair fair-syncer
  {:procs #{:fair-sync}
   :local {:pc ::visit}}
  {[::visit {}]
   (fn [db]
     (let [lib (nth visit-order (::rota db))
           db' (if (not= (get (::current db) lib) (get (::target db) lib))
                 (-> db
                     (assoc-in [::current lib] (get (::target db) lib))
                     (update ::synced conj lib))
                 db)]
       (update db' ::rota #(mod (inc %) (count visit-order)))))})

(rh/definvariant applied-stays-synced [db]
  (every? (fn [l] (= (get (::current db) l) (get (::target db) l)))
          (::synced db)))

(rh/definvariant emitted<=>drift [db]
  (let [drifted (filter (fn [l] (not= (get (::current db) l)
                                      (get (::target db) l)))
                        libs)]
    (= (boolean (seq drifted))
       (not= (::current db) (::target db)))))

(rh/defproperty eventually-all-synced [db]
  (rh/eventually (= (::current db) (::target db))))

(defn safety-spec []
  {:name       ::sync-safety
   :init-state global
   :components [syncer applied-stays-synced emitted<=>drift]
   :safety     ["applied-stays-synced" "emitted<=>drift"]
   :liveness   []})

(defn liveness-spec []
  {:name       ::sync-liveness
   :init-state global
   :components [fair-syncer eventually-all-synced]
   :safety     []
   :liveness   ["eventually-all-synced"]})

(defn- run-fn
  "Effect fn for hr/check!: run through recife with deadlock checking off —
   the converged fixpoint (current = target) is intended termination."
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
