(ns bb-depsolve.version.tree
  "Transitive dependency trees: building, conflict detection, resolution."
  (:require [clojure.set :as set]
            [malli.core :as m]
            [bb-depsolve.schema]
            [bb-depsolve.version.semver :as semver]))

(defn build-dep-tree
  "Build a transitive dependency tree from a list of direct deps.
   resolve-fn: (fn [lib version] -> [{:lib :version :type}]) returns children.
   max-depth: recursion limit (0 = direct deps only, no children resolved).
   seen: set of already-visited [lib version] to avoid cycles.
   Returns vec of tree nodes: {:lib :version :type :children [...] :cycle? bool}."
  ([deps resolve-fn max-depth]
   (build-dep-tree deps resolve-fn max-depth 0 #{}))
  ([deps resolve-fn max-depth current-depth seen]
   (->> deps
        (mapv (fn [{:keys [lib version] :as dep}]
                (let [key [lib version]]
                  (if (or (and max-depth (>= current-depth max-depth))
                          (contains? seen key))
                    (assoc dep :children [] :cycle? (contains? seen key))
                    (let [children (resolve-fn lib version)
                          child-tree (build-dep-tree children resolve-fn max-depth
                                                     (inc current-depth)
                                                     (conj seen key))]
                      (assoc dep :children child-tree :cycle? false)))))))))

(defn find-conflicts
  "Walk a dep tree, find libs appearing with multiple versions.
   Returns map of {lib -> #{versions}} for conflicting libs only."
  [trees]
  (let [versions (atom {})]
    (letfn [(walk [nodes]
              (doseq [{:keys [lib version children]} nodes]
                (swap! versions update lib (fnil conj #{}) version)
                (walk children)))]
      (walk trees))
    (->> @versions
         (filter (fn [[_ vs]] (> (count vs) 1)))
         (into {}))))

(defn collect-occurrences
  "Walk dep tree and collect every occurrence of each lib as
   {lib [{:version :type :depth} ...]}. Pure. Used by resolve-versions."
  [trees]
  (let [acc (atom {})]
    (letfn [(walk [nodes depth]
              (doseq [{:keys [lib version type children cycle?]} nodes]
                (when-not cycle?
                  (swap! acc update lib (fnil conj [])
                         {:version version :type type :depth depth}))
                (walk children (inc depth))))]
      (walk trees 0))
    @acc))

(defn resolve-versions
  "Maven-style nearest-wins resolver over a transitive dep tree.

   Picks ONE chosen version per lib using the occurrence with the lowest
   depth (root deps win over transitive). Ties at the same depth break by
   the highest version (per `version-compare`).

   Pure, total. Inputs:
     trees - vec of dep tree nodes (output of `build-dep-tree`)

   Returns a map:
     {:resolved  {lib {:version :type :depth}}   ; nearest-wins selection
      :conflicts {lib #{versions}}               ; libs with >1 distinct version
      :occurrences {lib [{:version :type :depth} ...]}  ; raw multiset
      :missing   #{libs}}                        ; libs ONLY appearing in cycles,
                                                 ; never resolved elsewhere

   Cycle, conflict, diamond and missing-dep cases are all surfaced through
   this single data structure; callers (Action layer) decide how to render."
  [trees]
  (let [occurrences (collect-occurrences trees)
        cycle-acc (atom #{})
        _ (letfn [(walk [nodes]
                    (doseq [{:keys [lib children cycle?]} nodes]
                      (when cycle? (swap! cycle-acc conj lib))
                      (walk children)))]
            (walk trees))
        cycle-libs @cycle-acc
        nearest (fn [occs]
                  ;; min depth wins; among ties, highest version wins
                  (->> occs
                       (group-by :depth)
                       (sort-by key)
                       first
                       val
                       (sort-by :version semver/version-compare)
                       last))
        resolved (into {}
                       (map (fn [[lib occs]] [lib (nearest occs)]))
                       occurrences)
        conflicts (->> occurrences
                       (filter (fn [[_ occs]]
                                 (> (count (set (map :version occs))) 1)))
                       (map (fn [[lib occs]] [lib (set (map :version occs))]))
                       (into {}))
        missing (set/difference cycle-libs (set (keys resolved)))]
    {:resolved resolved
     :conflicts conflicts
     :occurrences occurrences
     :missing missing}))

(m/=> resolve-versions
      [:=> [:cat [:vector :bb-depsolve/tree-node]] :bb-depsolve/resolution])

(defn format-dep-tree
  "Format dependency tree as indented string lines with ANSI colors.
   conflicts: map from find-conflicts. seen: tracks already-printed deps.
   Returns vec of formatted strings (one per line)."
  ([trees conflicts]
   (format-dep-tree trees conflicts 0 (atom #{})))
  ([trees conflicts indent-level seen]
   (let [indent (apply str (repeat (* 2 indent-level) \space))]
     (->> trees
          (mapcat (fn [{:keys [lib version children cycle?]}]
                    (let [conflict? (contains? conflicts lib)
                          seen? (contains? @seen [lib version])
                          _ (swap! seen conj [lib version])
                          version-str (cond
                                        conflict? (str "\033[33m" version "\033[0m")
                                        :else     (str "\033[2m" version "\033[0m"))
                          suffix (cond
                                   cycle?  (str " \033[2m(cycle)\033[0m")
                                   seen?   (str " \033[2m(already seen)\033[0m")
                                   :else   "")
                          line (str indent (str lib) " " version-str suffix)]
                      (if (or cycle? seen?)
                        [line]
                        (into [line]
                              (format-dep-tree children conflicts
                                               (inc indent-level) seen))))))
          (vec)))))
