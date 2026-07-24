(ns bb-depsolve.exec
  "Interpreting a cascade plan wave by wave.

   step-outcome {:project :status :version :tag :pin-updates :error}
   status       :released | :sync-failed | :release-failed
   run          {:status :waves [{:index :steps :await}] :released {project version}}"
  (:require [bb-depsolve.await :as await]
            [bb-depsolve.cascade :as cascade]
            [bb-depsolve.port :as port]
            [bb-depsolve.ui :as ui]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [bb-depsolve.schema :as sch]
            [bb-depsolve.schema.exec :as schema-exec]))

(defn preflight
  "=> Result of PLAN, or an :exec/unsafe-plan error when it carries cycles or
   unknown seeds. FORCE? admits it anyway."
  [plan force?]
  (let [cycles (vec (:cycles plan))
        unknown (vec (:unknown-seeds plan))]
    (if (or force? (and (empty? cycles) (empty? unknown)))
      (r/ok plan)
      (r/err {:kind :exec/unsafe-plan
              :cycles cycles
              :unknown-seeds unknown}))))

(defn resolve-pins
  "STEP with every nil :to filled from RELEASED {project version}."
  [step released]
  (update step :pin-updates
          (fn [pins]
            (mapv (fn [{:keys [dep coord to] :as pin}]
                    (cond-> pin
                      (nil? to)
                      (assoc :to (cascade/coord-version coord (get released dep)))))
                  pins))))

(defn- run-step!
  [port step]
  (let [project (:project step)
        synced (port/sync-pins! port step)]
    (if-not (r/ok? synced)
      {:project project :status :sync-failed :error (:error synced)}
      (let [applied (:applied (:ok synced))
            released (port/release! port step)]
        (if-not (r/ok? released)
          {:project project :status :release-failed
           :pin-updates applied :error (:error released)}
          {:project project :status :released
           :pin-updates applied
           :version (:version (:ok released))
           :tag (:tag (:ok released))})))))

(defn await-directive
  "WAVE's :await with each nil :expect filled from what OUTCOMES released."
  [wave outcomes]
  (let [versions (into {} (map (juxt :project :version)) outcomes)
        lib->project (into {} (map (juxt :lib :project)) (:steps wave))]
    (update (:await wave) :libs
            (fn [libs]
              (mapv (fn [{:keys [lib expect] :as entry}]
                      (cond-> entry
                        (nil? expect)
                        (assoc :expect (get versions (get lib->project lib)))))
                    libs)))))

(defn released?
  [outcome]
  (= :released (:status outcome)))

(schema-exec/register!)

(defn- run-value
  "Validated run record. Throws on a malformed run."
  [status waves released]
  (sch/validate! :bb-depsolve/exec-run
                 {:status status :waves waves :released released}))

(defn- default-emit
  [{:keys [type project outcome]}]
  (case type
    :step/start (do (print (format "  %-28s " project)) (flush))
    :step/end (println (if (released? outcome)
                         (ui/c :green (str "released " (:version outcome)))
                         (ui/c :red (name (:status outcome)))))
    :wave/start (println (ui/c :bold (format "wave %d" (:index outcome))))
    nil))

(defn format-failure
  "Failure text for an exec error value."
  [{:keys [kind cycles unknown-seeds failed await] :as error}]
  (case kind
    :exec/unsafe-plan
    (str (ui/c :red "refusing to execute an unsafe plan")
         (when (seq cycles)
           (str "\n  cycles: "
                (str/join "; " (map #(str/join " <-> " (sort %)) cycles))))
         (when (seq unknown-seeds)
           (str "\n  unknown seeds: " (str/join ", " unknown-seeds)))
         "\n  pass --force to execute the orderable remainder anyway.")

    :exec/step-failed
    (str (ui/c :red "aborted: a step failed")
         "\n  failed: " (str/join ", " failed))

    :exec/await-failed
    (await/format-timeout await)

    (str (ui/c :red "aborted: ") (pr-str error))))

(defn run-plan!
  "Execute PLAN: for each wave, sync every step's pins and release it through
   PORT, then await the wave's artifacts on REGISTRY before the next.

   OPTS: :force?     execute despite cycles or unknown seeds
         :await-opts forwarded to await/await-wave!
         :emit       (fn [event])
         :released   {project version} already published by an earlier run
         :on-wave    (fn [run]) called with the partial run after each wave

   => Result of the run. Every error value carries the partial :run."
  ([port registry plan] (run-plan! port registry plan {}))
  ([port registry plan opts]
   (r/let-ok [_ (preflight plan (:force? opts))]
     (let [emit (get opts :emit default-emit)
           on-wave (get opts :on-wave (constantly nil))]
       (loop [[wave & more] (:waves plan)
              released (get opts :released {})
              acc []]
         (if (nil? wave)
           (r/ok (run-value :complete acc released))
           (let [_ (emit {:type :wave/start :outcome wave})
                 outcomes (mapv (fn [step]
                                  (emit {:type :step/start :project (:project step)})
                                  (let [outcome (run-step! port (resolve-pins step released))]
                                    (emit {:type :step/end :outcome outcome})
                                    outcome))
                                (:steps wave))
                 released' (into released
                                 (keep #(when (released? %) [(:project %) (:version %)]))
                                 outcomes)
                 failed (remove released? outcomes)]
             (if (seq failed)
               (let [run (run-value :aborted
                                    (conj acc {:index (:index wave) :steps outcomes})
                                    released')]
                 (on-wave run)
                 (r/err {:kind :exec/step-failed
                         :failed (mapv :project failed)
                         :run run}))
               (let [awaited (await/await-wave! registry
                                                (await-directive wave outcomes)
                                                (get opts :await-opts {}))
                     record {:index (:index wave)
                             :steps outcomes
                             :await (if (r/ok? awaited) (:ok awaited) (:error awaited))}
                     acc2 (conj acc record)]
                 (if (r/ok? awaited)
                   (do (on-wave (run-value :running acc2 released'))
                       (recur more released' acc2))
                   (let [run (run-value :aborted acc2 released')]
                     (on-wave run)
                     (r/err {:kind :exec/await-failed
                             :await (:error awaited)
                             :run run}))))))))))))