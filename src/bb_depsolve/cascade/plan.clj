(ns bb-depsolve.cascade.plan
  "Transitive release planning over the internal dependency DAG.

   Pipeline layer: turns a seed set plus a graph into a release PLAN VALUE.
   Pure — no git, no network, no clock. The plan is the contract an executor
   interprets.

   Plan value:
     {:seeds         #{project}
      :unknown-seeds #{project}
      :policy        {:requested-bump kw :await await}
      :waves         [{:index n :steps [step] :await await}]
      :cycles        [#{project}]
      :excluded      [{:project p :reason kw}]}

   step:
     {:project :lib :dir :role :release-mode :current-version
      :bump-kind :next-version :pin-updates [pin-update]}

   pin-update:
     {:dep :lib :coord :path :from :to}

   await:
     {:mode :wait|:skip :timeout-ms n
      :libs [{:lib :newer-than :expect}]}"
  (:require [bb-depsolve.graph.dag :as graph]
            [bb-depsolve.version.api :as v]
            [bb-depsolve.cascade.bump :as bump]))

(def default-await-timeout-ms
  "Ceiling for waiting on one wave's artifacts to become resolvable."
  900000)

(def default-await
  {:mode :wait :timeout-ms default-await-timeout-ms})

(defn coord-version
  "VERSION rendered in COORD's shape: git coordinates carry a leading v,
   maven coordinates do not. Nil-safe."
  [coord version]
  (when version
    (if (= :git coord) (str "v" version) version)))

(defn pinned-versions
  "Versions consumers currently pin for PROJECT in G, normalized to maven form.
   Returns a set. Pure."
  [g project]
  (into #{}
        (comp (filter (fn [[[_ dep] _]] (= dep project)))
              (mapcat val)
              (keep :version)
              (map v/tag->mvn-version))
        (:pins g)))

(defn effective-version
  "Highest version PROJECT is known to be at: the version it declares, or the
   highest version any consumer pins, whichever is greater.

   Returns {:version v :declared d :observed o}, where :observed is the highest
   pinned version when it exceeds the declared one and nil otherwise. A
   non-nil :observed means the project's VERSION file lags its own releases."
  [g project declared]
  (let [pinned (pinned-versions g project)
        candidates (cond-> pinned declared (conj declared))
        highest (last (sort v/version-compare candidates))]
    {:version (or highest declared)
     :declared declared
     :observed (when (and declared highest (not= highest declared)) highest)}))

(defn- pin-updates
  "Pin updates PROJECT must take because its in-plan dependencies are released.
   DECIDED maps an already-planned project to its {:next-version ...}.
   :to is rendered in the pin's own coordinate shape and is nil when the
   dependency's next version is not predictable."
  [g project deps decided]
  (vec
   (for [dep (sort deps)
         pin (get-in g [:pins [project dep]])]
     {:dep dep
      :lib (or (get-in g [:nodes dep :lib]) (:lib pin))
      :coord (:coord pin)
      :path (:path pin)
      :from (:version pin)
      :to (coord-version (:coord pin) (get-in decided [dep :next-version]))})))

(defn- wave-await
  "Await directive for the artifacts STEPS produce, under POLICY."
  [policy steps]
  (assoc policy
         :libs (vec (for [{:keys [lib current-version next-version]} steps]
                      {:lib lib
                       :newer-than current-version
                       :expect next-version}))))

(defn- plan-step
  [g seed-set rules requested wave-index decided project]
  (let [node (get-in g [:nodes project])
        deps (graph/depends-on g project)
        role (if (contains? seed-set project) :seed :consumer)
        upstream (bump/strongest-bump (map #(get-in decided [% :bump-kind]) deps))
        ctx {:project project
             :role role
             :release-mode (:release-mode node)
             :requested-bump requested
             :upstream-bump upstream
             :wave-index wave-index}
        bump-kind (bump/select-bump-kind rules ctx)
        {:keys [version declared observed]} (effective-version g project (:version node))]
    (cond-> {:project project
             :lib (:lib node)
             :dir (:dir node)
             :role role
             :release-mode (:release-mode node)
             :current-version version
             :bump-kind bump-kind
             :next-version (bump/next-version version bump-kind)
             :pin-updates (pin-updates g project deps decided)}
      observed (assoc :version-drift {:declared declared :observed observed}))))

(defn plan-cascade
  "Ordered release plan propagating SEEDS through G.

   OPTS:
     :requested-bump  bump kind for seed projects (default :patch)
     :bump-rules      rule chain (default default-bump-rules)
     :await           {:mode :wait|:skip :timeout-ms n} (default default-await)

   Every wave contains only projects whose in-plan dependencies were released
   in an earlier wave. Projects on or behind a cycle are excluded, never
   silently reordered. Returns the plan value."
  ([g seeds] (plan-cascade g seeds {}))
  ([g seeds opts]
   (let [seed-set (set seeds)
         known (set (keys (:nodes g)))
         closure (graph/downstream-closure g seed-set)
         sub (graph/induced-subgraph g closure)
         {ws :waves cyclic :cyclic} (graph/waves sub)
         on-cycle (into #{} cat (graph/cycles sub))
         rules (get opts :bump-rules bump/default-bump-rules)
         requested (get opts :requested-bump :patch)
         await-policy (merge default-await (:await opts))]
     (loop [[wave & more] ws
            index 0
            decided {}
            acc []]
       (if (nil? wave)
         {:seeds seed-set
          :unknown-seeds (into (sorted-set) (remove known) seed-set)
          :policy {:requested-bump requested :await await-policy}
          :waves acc
          :cycles (graph/cycles sub)
          :excluded (vec (for [p cyclic]
                           {:project p
                            :reason (if (on-cycle p) :cycle-member :blocked-by-cycle)}))}
         (let [steps (mapv #(plan-step sub seed-set rules requested index decided %) wave)
               decided' (into decided
                              (map (juxt :project #(select-keys % [:bump-kind :next-version])))
                              steps)]
           (recur more
                  (inc index)
                  decided'
                  (conj acc {:index index
                             :steps steps
                             :await (wave-await await-policy steps)}))))))))

(defn plan-projects
  "Every project the plan releases, in wave order."
  [plan]
  (into [] (mapcat #(map :project (:steps %))) (:waves plan)))