(ns bb-depsolve.layer.check
  "Pure layer classification of an internal dependency graph."
  (:require [bb-depsolve.layer.rules :as rules]
            [bb-depsolve.layer.table :as table]))

(defn edge-context
  "Rule-chain context for the edge FROM -> TO under TABLE."
  [table from to]
  (let [from-level (table/level-of table from)
        to-level   (table/level-of table to)]
    {:from         from
     :to           to
     :from-level   from-level
     :to-level     to-level
     :to-terminal? (contains? (:terminal table) to-level)
     :waived?      (contains? (:waivers table) [from to])}))

(defn classify-edge
  "The edge FROM -> TO with its level names, verdict and waiver reason.

   RULES defaults to `rules/default-layer-rules`."
  ([table from to] (classify-edge rules/default-layer-rules table from to))
  ([rules table from to]
   (let [ctx (edge-context table from to)]
     (assoc ctx
            :from-layer (table/level-name table (:from-level ctx))
            :to-layer   (table/level-name table (:to-level ctx))
            :verdict    (rules/classify rules ctx)
            :reason     (get-in table [:reasons [from to]])))))

(defn classify-graph
  "Every edge in EDGES classified, ordered by consumer then dependency.

   EDGES is {consumer #{dependency}} — the :edges slot of an internal graph."
  ([table edges] (classify-graph rules/default-layer-rules table edges))
  ([rules table edges]
   (vec (for [from (sort (keys edges))
              to   (sort (get edges from))]
          (classify-edge rules table from to)))))

(defn violations
  "Only the classified edges whose verdict is :violation."
  [classified]
  (filterv #(= :violation (:verdict %)) classified))

(defn summary
  "{verdict -> count} over CLASSIFIED edges."
  [classified]
  (frequencies (map :verdict classified)))

(defn check
  "Classify EDGES under TABLE.

   Returns {:classified [...] :violations [...] :summary {...} :unranked [...]},
   where :unranked lists graph projects the table does not rank."
  ([table edges] (check rules/default-layer-rules table edges))
  ([rules table edges]
   (let [classified (classify-graph rules table edges)
         projects   (into (set (keys edges)) (mapcat val) edges)]
     {:classified classified
      :violations (violations classified)
      :summary    (summary classified)
      :unranked   (table/unranked table projects)})))
