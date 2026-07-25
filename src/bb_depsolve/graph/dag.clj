(ns bb-depsolve.graph.dag
  "Pure calculations over the workspace-internal dependency DAG.

   Calculation layer: total functions over plain data. No IO.

   Vocabulary:
     project — artifact-id of a workspace project, e.g. \"hive-weave\"
     node    — {:project :lib :dir :release-mode :version}
     edge    — project -> #{project}, read as \"depends on\"
     pin     — one coordinate one project holds on another, in one dep file

   Graph value:
     {:nodes {project node}
      :edges {project #{project}}
      :pins  {[consumer dependency] [pin ...]}}"
  (:require [clojure.set :as set]))

(defn dep-graph
  "Build the internal dependency graph from NODES and PINS.

   NODES: seq of {:project :lib :dir :release-mode :version}
   PINS:  seq of {:project :dep :lib :coord :version :path}, where :dep is the
          artifact-id depended upon.

   OPTS:
     :edge? predicate deciding which pins induce an ordering edge
            (default: every pin). Pins that fail it are still indexed under
            :pins, so they can be rewritten without constraining release order.

   Pins whose :project or :dep is not a known node are dropped. Self-pins are
   kept as self-edges. Every node gets an :edges key. Returns the graph value."
  ([nodes pins] (dep-graph nodes pins {}))
  ([nodes pins {:keys [edge?] :or {edge? (constantly true)}}]
   (let [node-index (into {} (map (juxt :project identity)) nodes)
         known? #(contains? node-index %)
         relevant (filter #(and (known? (:project %)) (known? (:dep %))) pins)]
     {:nodes node-index
      :edges (reduce (fn [m {:keys [project dep]}]
                       (update m project (fnil conj #{}) dep))
                     (zipmap (keys node-index) (repeat #{}))
                     (filter edge? relevant))
      :pins (reduce (fn [m {:keys [project dep] :as pin}]
                      (update m [project dep] (fnil conj []) pin))
                    {}
                    relevant)})))

(defn projects
  "Sorted vector of every project in G."
  [g]
  (vec (sort (keys (:nodes g)))))

(defn- scoped-edges
  "Edges of G with every target restricted to G's own nodes."
  [g]
  (let [nodes (set (keys (:nodes g)))]
    (into {} (for [n nodes]
               [n (set/intersection (get (:edges g) n #{}) nodes)]))))

(defn depends-on
  "Set of projects PROJECT depends on within G."
  [g project]
  (set/intersection (get (:edges g) project #{})
                    (set (keys (:nodes g)))))

(defn reverse-edges
  "Invert G's edges. Returns {project #{dependent-project}}; every node has a key."
  [g]
  (reduce-kv (fn [m p deps]
               (reduce (fn [m' d] (update m' d (fnil conj #{}) p)) m deps))
             (zipmap (keys (:nodes g)) (repeat #{}))
             (scoped-edges g)))

(defn dependents
  "Set of projects that depend on PROJECT within G."
  [g project]
  (get (reverse-edges g) project #{}))

(defn downstream-closure
  "All projects transitively depending on any of SEEDS, including SEEDS.
   Seeds absent from G are retained. Terminates on cycles."
  [g seeds]
  (let [rdeps (reverse-edges g)]
    (loop [frontier (set seeds)
           seen (set seeds)]
      (let [next-frontier (into #{} (comp (mapcat #(get rdeps % #{}))
                                          (remove seen))
                                frontier)]
        (if (empty? next-frontier)
          seen
          (recur next-frontier (set/union seen next-frontier)))))))

(defn induced-subgraph
  "Restrict G to PROJECTS, dropping other nodes, edges and pins."
  [g projects]
  (let [keep? (set projects)]
    {:nodes (into {} (filter (comp keep? key)) (:nodes g))
     :edges (into {} (for [[p deps] (:edges g) :when (keep? p)]
                       [p (into #{} (filter keep?) deps)]))
     :pins (into {} (for [[[c d :as k] ps] (:pins g)
                          :when (and (keep? c) (keep? d))]
                      [k ps]))}))

(defn waves
  "Partition G into dependency levels: every project in wave n has all of its
   in-graph dependencies in waves < n. Wave members are name-sorted.

   Returns {:waves [[project ...] ...] :cyclic #{project ...}}. Projects that
   cannot be ordered (cycle members and anything blocked by a cycle) are
   excluded from :waves and collected in :cyclic."
  [g]
  (loop [remaining (scoped-edges g)
         acc []]
    (let [ready (->> remaining
                     (keep (fn [[p deps]] (when (empty? deps) p)))
                     sort
                     vec)]
      (if (empty? ready)
        {:waves acc :cyclic (into (sorted-set) (keys remaining))}
        (let [ready-set (set ready)]
          (recur (reduce-kv (fn [m p deps]
                              (if (ready-set p)
                                m
                                (assoc m p (set/difference deps ready-set))))
                            {}
                            remaining)
                 (conj acc ready)))))))

(defn topo-order
  "Dependency-first ordering of G.
   Returns {:order [project ...] :cyclic #{project ...}}."
  [g]
  (let [{ws :waves cyc :cyclic} (waves g)]
    {:order (vec (apply concat ws))
     :cyclic cyc}))

(defn- reachable
  "Projects reachable from START by following EDGES within SCOPE.
   Includes START only when START lies on a cycle."
  [edges scope start]
  (loop [frontier (set/intersection (get edges start #{}) scope)
         seen #{}]
    (let [fresh (set/difference frontier seen)]
      (if (empty? fresh)
        seen
        (recur (into #{} (mapcat #(set/intersection (get edges % #{}) scope)) fresh)
               (set/union seen fresh))))))

(defn cycles
  "Cycle components of G: sets of mutually reachable projects, self-loops
   included. Projects merely blocked by a cycle are not reported.
   Returns a vector of sets ordered by their smallest member; empty when G is
   acyclic."
  [g]
  (let [{:keys [cyclic]} (waves g)
        edges (:edges g)
        reach (into {} (map (juxt identity #(reachable edges cyclic %))) cyclic)
        on-cycle (filterv #(contains? (reach %) %) cyclic)]
    (->> on-cycle
         (map (fn [p]
                (into (sorted-set p)
                      (filter #(and (contains? (reach p) %)
                                    (contains? (reach %) p)))
                      on-cycle)))
         distinct
         (sort-by first)
         vec)))

(defn blocked
  "Projects G cannot order because they depend on a cycle, excluding the cycle
   members themselves."
  [g]
  (let [{:keys [cyclic]} (waves g)
        on-cycle (into #{} cat (cycles g))]
    (into (sorted-set) (remove on-cycle) cyclic)))
