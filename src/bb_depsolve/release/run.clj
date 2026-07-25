(ns bb-depsolve.release.run
  "Coordinated-release commands over the internal dependency DAG.

   Boundary layer: collects the workspace, delegates every decision to the
   pure bb-depsolve.graph.dag and bb-depsolve.cascade.plan calculations, and renders
   the result. Writes nothing."
  (:require [babashka.fs :as fs]
            [bb-depsolve.cascade.plan :as cas]
            [bb-depsolve.graph.collect :as col]
            [bb-depsolve.release.exec :as exec]
            [bb-depsolve.release.git-port :as git-port]
            [bb-depsolve.release.reconcile :as reconcile]
            [bb-depsolve.registry.live :as registry]
            [bb-depsolve.release.opts :as ropts]
            [bb-depsolve.release.render :as render]
            [bb-depsolve.release.resume :as resume]
            [bb-depsolve.cli.ui :as ui]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn- execute-cascade!
  "Run PLAN for real, resuming from ROOT-DIR's checkpoint when one is present.
   Clears the checkpoint on a complete run and keeps it on an aborted one."
  [root-dir plan {:keys [force? no-wait await-timeout]}]
  (let [prior (resume/load-run root-dir)
        done (resume/released prior)
        todo (resume/remaining plan done)]
    (when-let [note (resume/describe prior)]
      (println (ui/c :dim note)))
    (if (empty? (:waves todo))
      (do (resume/clear! root-dir)
          (println (ui/c :green "Nothing left to release: the checkpoint covers the whole plan.")))
      (let [result (exec/run-plan! (git-port/git-port)
                                   (registry/live-registry)
                                   todo
                                   {:force? force?
                                    :released done
                                    :on-wave #(resume/save! root-dir %)
                                    :await-opts (cond-> {}
                                                  no-wait (assoc :mode :skip)
                                                  await-timeout (assoc :timeout-ms await-timeout))})]
        (if-let [run (:ok result)]
          (do (resume/clear! root-dir)
              (println)
              (println (ui/c :green (format "Cascade complete: %d project(s) released."
                                            (count (:released run)))))
              (doseq [[p v] (sort (:released run))]
                (printf "  %-26s %s\n" (ui/c :cyan p) (ui/c :green v))))
          (let [{:keys [error]} result]
            (println)
            (println (exec/format-failure error))
            (when-let [run (:run error)]
              (println (ui/c :dim (format "  %d project(s) already released; re-run to resume."
                                          (count (:released run))))))
            (System/exit 1)))))))

(defn cascade-cmd
  "Plan a transitive release cascade across the workspace.

   Seeds come from --from <csv>; without it, every project holding unpublished
   commits seeds the cascade. Planning only unless --apply is passed."
  [{:keys [opts]}]
  (let [{:keys [root org skip-dirs depth from no-wait await-timeout apply force]
         out-format :format
         :or {root "."}} opts
        out-format (or out-format "text")
        skips (ropts/skip-set skip-dirs)
        root-dir (str (fs/canonicalize root))]
    (ropts/require-org! org "cascade")
    (let [g (col/collect-graph {:root root-dir :skip-dirs skips :depth depth :org org})
          seeds (or (ropts/parse-seeds from)
                    (col/detect-seeds (col/collect-nodes root-dir skips org)))]
      (if (empty? seeds)
        (println (ui/c :green "Nothing to cascade: every project is published up to date."))
        (let [plan (cas/plan-cascade g seeds
                                     {:requested-bump (ropts/requested-bump opts)
                                      :await (cond-> {}
                                               no-wait (assoc :mode :skip)
                                               await-timeout (assoc :timeout-ms await-timeout))})]
          (cond
            (= out-format "edn") (pp/pprint plan)

            apply
            (do (render/print-plan plan)
                (println)
                (println (ui/c :bold "Executing — this bumps, tags and pushes."))
                (println)
                (execute-cascade! root-dir plan {:force? force
                                                 :no-wait no-wait
                                                 :await-timeout await-timeout}))

            :else (render/print-plan plan)))))))

(defn reconcile-cmd
  "Rewrite VERSION files that lag the versions already evidenced by their own
   git tags or by the pins their consumers carry.

   Reports only unless --apply is passed."
  [{:keys [opts]}]
  (let [{:keys [root org skip-dirs depth apply]
         :or {root "."}} opts
        skips (ropts/skip-set skip-dirs)
        root-dir (str (fs/canonicalize root))]
    (ropts/require-org! org "reconcile")
    (let [g (col/collect-graph {:root root-dir :skip-dirs skips :depth depth :org org})
          dir-of #(str (fs/path root-dir (get-in g [:nodes % :dir] %)))
          drifts (reconcile/survey g dir-of)]
      (if (empty? drifts)
        (println (ui/c :green "No VERSION drift: every declared version is the highest evidenced."))
        (do
          (println (ui/c :bold (format "%d project(s) declare a version below what they have released:"
                                       (count drifts))))
          (println)
          (doseq [{:keys [project declared highest from-tags from-pins]} drifts]
            (printf "  %-26s %s -> %s   %s\n"
                    (ui/c :cyan project)
                    (ui/c :red declared)
                    (ui/c :green highest)
                    (ui/c :dim (str/join ", "
                                         (cond-> []
                                           from-tags (conj (str "tag " from-tags))
                                           from-pins (conj (str "pinned " from-pins)))))))
          (println)
          (if apply
            (doseq [{:keys [project] :as d} drifts]
              (let [written (reconcile/apply-drift! (dir-of project) d)]
                (println (ui/c :green (format "  %s — wrote %d VERSION file(s)"
                                              project (count written))))))
            (println (ui/c :dim "  Dry run. Pass --apply to rewrite these VERSION files."))))))))