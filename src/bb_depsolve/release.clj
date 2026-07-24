(ns bb-depsolve.release
  "Coordinated-release commands over the internal dependency DAG.

   Boundary layer: collects the workspace, delegates every decision to the
   pure bb-depsolve.graph and bb-depsolve.cascade calculations, and renders
   the result. Writes nothing."
  (:require [babashka.fs :as fs]
            [bb-depsolve.cascade :as cas]
            [bb-depsolve.collect :as col]
            [bb-depsolve.core :as core]
            [bb-depsolve.graph :as graph]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn- skip-set
  [skip-dirs]
  (if skip-dirs
    (into #{} (str/split skip-dirs #","))
    core/default-skip-dirs))

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
    (println (core/c :red (format "Error: --org is required for %s (e.g. --org hive-agi)" cmd)))
    (System/exit 1)))

(defn- print-cycles
  [g]
  (let [cycles (graph/cycles g)
        blocked (graph/blocked g)]
    (when (seq cycles)
      (println)
      (println (core/c :red (format "%d dependency cycle(s) — these cannot be ordered:" (count cycles))))
      (doseq [c cycles]
        (println (str "  " (core/c :yellow (str/join " <-> " (sort c)))))))
    (when (seq blocked)
      (println)
      (println (core/c :yellow (format "%d project(s) blocked behind a cycle:" (count blocked))))
      (println (str "  " (core/c :dim (str/join " " blocked)))))))

(defn- print-unlinked
  [dep-files nodes]
  (let [unlinked (col/collect-unlinked-pins dep-files nodes)]
    (when (seq unlinked)
      (println)
      (println (core/c :yellow
                       (format "%d internal pin(s) use a bare :git/sha — no version to compare, no edge derived:"
                               (count unlinked))))
      (doseq [{:keys [project dep sha]} (distinct unlinked)]
        (printf "  %-24s -> %-24s %s\n"
                (core/c :cyan project) dep (core/c :dim (subs sha 0 (min 7 (count sha)))))))))

(defn- graph->dot
  [g]
  (str "digraph deps {\n  rankdir=LR;\n"
       (str/join (for [p (graph/projects g)
                       d (sort (graph/depends-on g p))]
                   (format "  \"%s\" -> \"%s\";\n" p d)))
       "}\n"))

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
        "dot" (print (graph->dot g))
        (do
          (println (core/c :bold
                           (format "%d internal projects, %d ordering edges, %d release levels"
                                   (count (:nodes g))
                                   (reduce + (map count (vals (:edges g))))
                                   (count ws))))
          (println)
          (doseq [[i wave] (map-indexed vector ws)]
            (println (core/c :bold (format "  Level %d  (%d)" i (count wave))))
            (doseq [p wave
                    :let [node (get-in g [:nodes p])
                          deps (sort (graph/depends-on g p))]]
              (printf "    %-26s %-9s %s%s\n"
                      (core/c :cyan p)
                      (core/c :dim (or (:version node) "?"))
                      (core/c :dim (name (:release-mode node)))
                      (if (seq deps)
                        (core/c :dim (str "  <- " (str/join " " deps)))
                        ""))))
          (print-cycles g)
          (print-unlinked (core/find-dep-files {:root root-dir
                                                :skip-dirs skips
                                                :depth (or depth core/default-depth)})
                          (col/collect-nodes root-dir skips org))
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
      (println (core/c :red "Error: --lib <project> is required for impact"))
      (System/exit 1))
    (let [g (col/collect-graph {:root root :skip-dirs skips :depth depth :org org})]
      (if-not (contains? (:nodes g) target)
        (do (println (core/c :red (format "Unknown project '%s'. Known: %s"
                                          target (str/join " " (graph/projects g)))))
            (System/exit 1))
        (let [plan (cas/plan-cascade g #{target})
              releases (cas/plan-projects plan)
              direct (graph/dependents g target)]
          (println (core/c :bold (format "Releasing %s forces %d downstream release(s) across %d wave(s)."
                                         target (dec (count releases)) (count (:waves plan)))))
          (println)
          (println (core/c :bold "Direct consumers:"))
          (if (empty? direct)
            (println (core/c :dim "  none"))
            (doseq [d (sort direct)] (println (str "  " (core/c :cyan d)))))
          (println)
          (println (core/c :bold "Release order:"))
          (doseq [w (:waves plan)]
            (printf "  L%d  %s\n" (:index w)
                    (str/join " " (map #(core/c :cyan (:project %)) (:steps w)))))
          (when (seq (:excluded plan))
            (println)
            (println (core/c :yellow (format "%d project(s) excluded (cycle):" (count (:excluded plan)))))
            (doseq [{:keys [project reason]} (:excluded plan)]
              (printf "  %-26s %s\n" (core/c :cyan project) (core/c :dim (name reason)))))
          (println))))))

(defn- print-plan
  [plan]
  (let [{:keys [mode timeout-ms]} (get-in plan [:policy :await])]
    (println (core/c :bold (format "Cascade plan — %d release(s) in %d wave(s)"
                                   (count (cas/plan-projects plan))
                                   (count (:waves plan)))))
    (printf "  seeds: %s\n" (core/c :cyan (str/join " " (sort (:seeds plan)))))
    (printf "  bump:  %s   await: %s\n"
            (core/c :dim (name (get-in plan [:policy :requested-bump])))
            (core/c :dim (if (= :wait mode)
                           (format "wait up to %ds per wave" (quot timeout-ms 1000))
                           "skipped (--no-wait)")))
    (when (seq (:unknown-seeds plan))
      (println (core/c :yellow (format "  unknown seeds ignored: %s"
                                       (str/join " " (:unknown-seeds plan))))))
    (println)
    (doseq [w (:waves plan)]
      (println (core/c :bold (format "  Wave %d" (:index w))))
      (doseq [{:keys [project role release-mode current-version next-version
                      bump-kind pin-updates version-drift]} (:steps w)]
        (printf "    %-26s %-9s %s\n"
                (core/c :cyan project)
                (core/c :dim (name role))
                (if (= :rolling release-mode)
                  (core/c :dim (format "%s -> rolling (the push mints the version)" current-version))
                  (format "%s -> %s  (%s)"
                          (core/c :red current-version)
                          (core/c :green (or next-version "?"))
                          (name bump-kind))))
        (when version-drift
          (println (core/c :yellow
                           (format "        VERSION file says %s but consumers already pin %s — planning from %s"
                                   (:declared version-drift)
                                   (:observed version-drift)
                                   current-version))))
        (doseq [{:keys [dep coord path from to]} pin-updates]
          (printf "        pin %-22s %s -> %s  %s\n"
                  dep
                  (core/c :red from)
                  (if to (core/c :green to) (core/c :dim "resolved on release"))
                  (core/c :dim (format "(%s %s)" (name coord) (fs/file-name path))))))
      (when (= :wait mode)
        (println (core/c :dim (format "    wait for: %s"
                                      (str/join " " (map (comp str :lib) (:libs (:await w))))))))
      (println))
    (when (seq (:cycles plan))
      (println (core/c :red "  Cycles blocking part of the cascade:"))
      (doseq [c (:cycles plan)]
        (println (str "    " (core/c :yellow (str/join " <-> " (sort c)))))))
    (when (seq (:excluded plan))
      (println (core/c :yellow (format "  %d project(s) excluded:" (count (:excluded plan)))))
      (doseq [{:keys [project reason]} (:excluded plan)]
        (printf "    %-26s %s\n" (core/c :cyan project) (core/c :dim (name reason)))))))

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
      (println (core/c :red "cascade --apply is not implemented: this build plans only."))
      (println (core/c :dim "  Execute the plan with sync/bump per wave, or pipe --format edn to a runner."))
      (System/exit 1))
    (let [g (col/collect-graph {:root root-dir :skip-dirs skips :depth depth :org org})
          seeds (or (parse-seeds from)
                    (col/detect-seeds (col/collect-nodes root-dir skips org)))]
      (if (empty? seeds)
        (println (core/c :green "Nothing to cascade: every project is published up to date."))
        (let [plan (cas/plan-cascade g seeds
                                     {:requested-bump (requested-bump opts)
                                      :await (cond-> {}
                                               no-wait (assoc :mode :skip)
                                               await-timeout (assoc :timeout-ms await-timeout))})]
          (if (= out-format "edn")
            (pp/pprint plan)
            (print-plan plan)))))))
