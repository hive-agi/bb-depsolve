(ns bb-depsolve.release.inspect
  (:require [babashka.fs :as fs]
            [bb-depsolve.cascade :as cas]
            [bb-depsolve.collect :as col]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.graph :as graph]
            [bb-depsolve.release.opts :as ropts]
            [bb-depsolve.release.render :as render]
            [bb-depsolve.ui :as ui]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(declare graph-cmd impact-cmd)

(defn graph-cmd
  "Print the workspace-internal dependency DAG in release order.
   --format tree (default) | edn | dot."
  [{:keys [opts]}]
  (let [{:keys [root org skip-dirs depth]
         out-format :format
         :or {root "."}} opts
        out-format (or out-format "tree")
        skips (ropts/skip-set skip-dirs)
        root-dir (str (fs/canonicalize root))]
    (ropts/require-org! org "graph")
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
        skips (ropts/skip-set skip-dirs)]
    (ropts/require-org! org "impact")
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
