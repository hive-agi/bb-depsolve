(ns bb-depsolve.release
  "Coordinated-release commands over the internal dependency DAG.

   Boundary layer: collects the workspace, delegates every decision to the
   pure bb-depsolve.graph and bb-depsolve.cascade calculations, and renders
   the result. Writes nothing."
  (:require [babashka.fs :as fs]
            [bb-depsolve.cascade :as cas]
            [bb-depsolve.collect :as col]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.graph :as graph]
            [bb-depsolve.ui :as ui]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [bb-depsolve.release.render :as render]))

(defn- skip-set
  [skip-dirs]
  (if skip-dirs
    (into #{} (str/split skip-dirs #","))
    discovery/default-skip-dirs))

(defn- requested-bump
  "Bump kind a set of CLI flags asks for. Mirrors bump-cmd's flag mapping:
   --stable raises the major segment, --major the minor, everything else the
   patch."
  [{:keys [major stable]}]
  (cond stable :major
        major :minor
        :else :patch))

(defn- parse-seeds
  "Seed project names from a comma-separated --from value. Nil when absent."
  [from]
  (when (and from (not (str/blank? from)))
    (into (sorted-set) (remove str/blank?) (map str/trim (str/split from #",")))))

(defn- require-org!
  [org cmd]
  (when-not org
    (println (ui/c :red (format "Error: --org is required for %s (e.g. --org hive-agi)" cmd)))
    (System/exit 1)))

(defn graph-cmd
  "Print the workspace-internal dependency DAG in release order.
   --format tree (default) | edn | dot."
  [{:keys [opts]}]
  (let [{:keys [root org skip-dirs depth]
         out-format :format
         :or {root "."}} opts
        out-format (or out-format "tree")
        skips (skip-set skip-dirs)
        root-dir (str (fs/canonicalize root))]
    (require-org! org "graph")
    (let [g (col/collect-graph {:root root-dir :skip-dirs skips :depth depth :org org})
          {ws :waves} (graph/waves g)]
      (case out-format
        "edn" (pp/pprint g)
        "dot" (print (render/graph->dot g))
        (do
          (println (ui/c :bold
                           (format "%d internal projects, %d ordering edges, %d release levels"
                                   (count (:nodes g))
                                   (reduce + (map count (vals (:edges g))))
                                   (count ws))))
          (println)
          (doseq [[i wave] (map-indexed vector ws)]
            (println (ui/c :bold (format "  Level %d  (%d)" i (count wave))))
            (doseq [p wave
                    :let [node (get-in g [:nodes p])
                          deps (sort (graph/depends-on g p))]]
              (printf "    %-26s %-9s %s%s\n"
                      (ui/c :cyan p)
                      (ui/c :dim (or (:version node) "?"))
                      (ui/c :dim (name (:release-mode node)))
                      (if (seq deps)
                        (ui/c :dim (str "  <- " (str/join " " deps)))
                        ""))))
          (render/print-cycles g)
          (render/print-unlinked (discovery/find-dep-files {:root root-dir
                                                :skip-dirs skips
                                                :depth (or depth discovery/default-depth)}) (col/collect-nodes root-dir skips org))
          (println))))))

(defn impact-cmd
  "Show what releasing LIB would force downstream.
   Pass the project name via --lib (or as the first argument)."
  [{:keys [opts args]}]
  (let [{:keys [root org skip-dirs depth lib]
         :or {root "."}} opts
        target (or lib (first args))
        skips (skip-set skip-dirs)]
    (require-org! org "impact")
    (when-not target
      (println (ui/c :red "Error: --lib <project> is required for impact"))
      (System/exit 1))
    (let [g (col/collect-graph {:root root :skip-dirs skips :depth depth :org org})]
      (if-not (contains? (:nodes g) target)
        (do (println (ui/c :red (format "Unknown project '%s'. Known: %s"
                                          target (str/join " " (graph/projects g)))))
            (System/exit 1))
        (let [plan (cas/plan-cascade g #{target})
              releases (cas/plan-projects plan)
              direct (graph/dependents g target)]
          (println (ui/c :bold (format "Releasing %s forces %d downstream release(s) across %d wave(s)."
                                         target (dec (count releases)) (count (:waves plan)))))
          (println)
          (println (ui/c :bold "Direct consumers:"))
          (if (empty? direct)
            (println (ui/c :dim "  none"))
            (doseq [d (sort direct)] (println (str "  " (ui/c :cyan d)))))
          (println)
          (println (ui/c :bold "Release order:"))
          (doseq [w (:waves plan)]
            (printf "  L%d  %s\n" (:index w)
                    (str/join " " (map #(ui/c :cyan (:project %)) (:steps w)))))
          (when (seq (:excluded plan))
            (println)
            (println (ui/c :yellow (format "%d project(s) excluded (cycle):" (count (:excluded plan)))))
            (doseq [{:keys [project reason]} (:excluded plan)]
              (printf "  %-26s %s\n" (ui/c :cyan project) (ui/c :dim (name reason)))))
          (println))))))

(defn cascade-cmd
  "Plan a transitive release cascade across the workspace.

   Seeds come from --from <csv>; without it, every project holding unpublished
   commits seeds the cascade. Planning only — this command never writes."
  [{:keys [opts]}]
  (let [{:keys [root org skip-dirs depth from no-wait await-timeout apply]
         out-format :format
         :or {root "."}} opts
        out-format (or out-format "text")
        skips (skip-set skip-dirs)
        root-dir (str (fs/canonicalize root))]
    (require-org! org "cascade")
    (when apply
      (println (ui/c :red "cascade --apply is not implemented: this build plans only."))
      (println (ui/c :dim "  Execute the plan with sync/bump per wave, or pipe --format edn to a runner."))
      (System/exit 1))
    (let [g (col/collect-graph {:root root-dir :skip-dirs skips :depth depth :org org})
          seeds (or (parse-seeds from)
                    (col/detect-seeds (col/collect-nodes root-dir skips org)))]
      (if (empty? seeds)
        (println (ui/c :green "Nothing to cascade: every project is published up to date."))
        (let [plan (cas/plan-cascade g seeds
                                     {:requested-bump (requested-bump opts)
                                      :await (cond-> {}
                                               no-wait (assoc :mode :skip)
                                               await-timeout (assoc :timeout-ms await-timeout))})]
          (if (= out-format "edn")
            (pp/pprint plan)
            (render/print-plan plan)))))))
