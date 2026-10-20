(ns bb-depsolve.version.tree
  "Transitive dependency trees: building, conflict detection, resolution."
  (:require [clojure.set :as set]
            [malli.core :as m]
            [bb-depsolve.schema.api]
            [bb-depsolve.version.semver :as semver]))

(defn build-dep-tree
  "Build a transitive dependency tree from a list of direct deps.
   resolve-fn: (fn [lib version] -> [{:lib :version :type}]) returns children.
   max-depth: recursion limit (0 = direct deps only, no children resolved).
   seen: set of already-visited [lib version] to avoid cycles.
   Returns vec of tree nodes: {:lib :version :type :children [...] :cycle? bool}.

   OPTS (4- and 6-arity) carries :map-fn, (fn [f coll] -> positional results),
   which runs one level's resolve-fn calls. The default is `mapv`, so this
   namespace stays pure; the tree command injects a bounded parallel map so
   sibling POM fetches overlap instead of running one round-trip at a time.
   Only the resolve calls go through map-fn, and the recursion happens outside
   it, so a fan-out never nests inside another one and each item's timeout
   covers exactly one fetch. A map-fn may answer nil for an item it could not
   finish (a timeout or a throw); that dep is kept in place with :children []
   and :unresolved? true, so a failure is reported where it happened rather
   than filtered out of the tree."
  ([deps resolve-fn max-depth]
   (build-dep-tree deps resolve-fn max-depth {}))
  ([deps resolve-fn max-depth opts]
   (build-dep-tree deps resolve-fn max-depth 0 #{} opts))
  ([deps resolve-fn max-depth current-depth seen]
   (build-dep-tree deps resolve-fn max-depth current-depth seen {}))
  ([deps resolve-fn max-depth current-depth seen {:keys [map-fn] :as opts}]
   (let [map-fn  (or map-fn mapv)
         deps    (vec deps)
         stop?   (fn [{:keys [lib version]}]
                   (or (and max-depth (>= current-depth max-depth))
                       (contains? seen [lib version])))
         to-open (filterv (complement stop?) deps)
         ;; One positional result per opened dep: {:children [...]} or nil.
         fetched (zipmap (map (juxt :lib :version) to-open)
                         (map-fn (fn [{:keys [lib version]}]
                                   {:children (resolve-fn lib version)})
                                 to-open))]
     (mapv (fn [{:keys [lib version] :as dep}]
             (let [key [lib version]]
               (if (stop? dep)
                 (assoc dep :children [] :cycle? (contains? seen key))
                 (if-let [{:keys [children]} (get fetched key)]
                   (assoc dep
                          :children (build-dep-tree children resolve-fn max-depth
                                                    (inc current-depth)
                                                    (conj seen key)
                                                    opts)
                          :cycle? false)
                   (assoc dep :children [] :cycle? false :unresolved? true)))))
           deps))))

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
