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
            [bb-depsolve.release.pr :as pr]
            [bb-depsolve.release.render :as render]
            [bb-depsolve.release.resume :as resume]
            [bb-depsolve.cli.ui :as ui]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [bb-depsolve.release.port :as port]
            [bb-depsolve.graph.dag :as graph]
            [hive-weave.parallel :as par]
            [hive-dsl.result :as r]
            [bb-depsolve.core.git :as git]))

(defn report-release-pr!
  "Print one line for PROJECT's `pr/open-release-pr!` RESULT and return RESULT.
   Anything but a created or existing pull request is a warning, never a
   failure: the release it follows is already pushed, and the pull request can
   still be opened by hand."
  [project result]
  (let [{:keys [status url reason message]} (:ok result)]
    (println
     (cond
       (not (r/ok? result))
       (ui/c :red (format "  %-26s release PR failed: %s" project
                          (let [error (:error result)]
                            (if (str/blank? (str (:message error)))
                              (pr-str error)
                              (:message error)))))

       (#{:created :exists} status)
       (format "  %-26s %s %s" (ui/c :cyan project)
               (ui/c :green (if (= :created status) "release PR opened:" "release PR exists:"))
               url)

       (= :not-off-default reason)
       (ui/c :dim (format "  %-26s no release PR (%s)" project message))

       :else
       (ui/c :yellow (format "  %-26s release PR skipped: %s" project message))))
    result))

(defn release-pr!
  "Open the release pull request for the repository in DIR through OPEN-PR!,
   the forge port (fn [dir opts] -> Result), by default `pr/open-release-pr!`,
   and print its outcome. Never throws: an exception becomes an error value,
   because the release it follows is already pushed.
   OPTS as `pr/open-release-pr!`: :tag :root :checkpoint ...
   => the Result."
  ([dir opts] (release-pr! pr/open-release-pr! dir opts))
  ([open-pr! dir opts]
   (report-release-pr!
    (str (fs/file-name dir))
    (try (open-pr! dir opts)
         (catch Exception e
           (r/err {:kind :pr/threw :message (ex-message e)}))))))

(defn open-release-prs!
  "Open the release pull request of every project a complete cascade released
   (M4), through OPEN-PR! as `release-pr!`. `pr/open-release-pr!` skips a
   project released on its default branch, so only the ones that landed on a
   release branch get a pull request, and a missing or unauthenticated gh is a
   warning.

   PLAN supplies each project's :dir. CHECKPOINT is the in-memory run record,
   earlier waves included, handed over as the M1.3 sync-ripple source because
   the saved checkpoint is cleared when the cascade completes. ROOT-DIR is the
   workspace root that holds .bb-depsolve.edn.
   => [{:project :dir :result}] in release order."
  [open-pr! root-dir plan checkpoint]
  (let [dirs (into {} (for [wave (:waves plan)
                            step (:steps wave)]
                        [(:project step) (:dir step)]))]
    (vec (for [wave (:waves checkpoint)
               {:keys [project status tag version]} (:steps wave)
               :let [dir (get dirs project)]
               :when (and (= :released status) dir)]
           {:project project
            :dir dir
            :result (release-pr! open-pr! dir {:root root-dir
                                               :tag (or tag version)
                                               :checkpoint checkpoint})}))))

(defn- execute-cascade!
  "Run PLAN for real, resuming from ROOT-DIR's checkpoint when one is present.
   Clears the checkpoint on a complete run and keeps it on an aborted one.
   With RELEASE-PR?, a complete run then opens the release pull requests
   (`open-release-prs!`) through OPEN-PR!, default `pr/open-release-pr!`."
  [root-dir plan {:keys [force? no-wait await-timeout release-pr? open-pr!]}]
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
                (printf "  %-26s %s\n" (ui/c :cyan p) (ui/c :green v)))
              (when release-pr?
                (println)
                (println (ui/c :bold "Release pull requests:"))
                (open-release-prs! (or open-pr! pr/open-release-pr!) root-dir plan
                                   (update run :waves #(into (vec (:waves prior)) %)))))
          (let [{:keys [error]} result]
            (println)
            (println (exec/format-failure error))
            (when-let [run (:run error)]
              (println (ui/c :dim (format "  %d project(s) already released; re-run to resume."
                                          (count (:released run))))))
            (System/exit 1)))))))

(def ^:private published-concurrency
  "Simultaneous registry lookups while planning: each is a git ls-remote plus
   a few registry GETs."
  8)

(defn- published-version
  "Newest version of LIB REGISTRY can observe, or nil when it answers none."
  [registry lib]
  (let [result (port/latest-version registry lib)]
    (when (r/ok? result) (:ok result))))

(defn with-published
  "G with :published-version set on every node in PROJECTS that REGISTRY
   observes a version for, looked up with bounded fan-out."
  [g registry projects]
  (let [projects (vec (sort (filter #(get-in g [:nodes %]) projects)))
        versions (par/bounded-pmap {:concurrency published-concurrency
                                    :timeout-ms 60000
                                    :fallback nil}
                                   #(published-version registry (get-in g [:nodes % :lib]))
                                   projects)]
    (reduce (fn [g' [p version]]
              (cond-> g' version (assoc-in [:nodes p :published-version] version)))
            g
            (map vector projects versions))))

(defn cascade-plan
  "The cascade plan for OPTS ({:root :org :skip-dirs :depth :from ...}),
   observing published versions through REGISTRY (an IArtifactRegistry).
   Returns the plan value, or nil when there is nothing to cascade.

   Each project is planned from the highest of its VERSION, its newest tag,
   the newest version REGISTRY observes and its consumers' pins, so a stale
   VERSION never re-plans a release that already exists. Seeds are projects
   with commits past their release base. Alias-scoped pins order releases
   too, wherever they do not close a cycle: the consumer re-pins after its
   dependency publishes, so it needs a wave."
  [opts registry]
  (let [{:keys [root org skip-dirs depth from no-wait await-timeout]
         :or {root "."}} opts
        skips (ropts/skip-set skip-dirs)
        root-dir (str (fs/canonicalize root))
        g (-> (col/collect-graph {:root root-dir :skip-dirs skips :depth depth :org org})
              (graph/admit-edges (constantly true)))
        seeds (or (ropts/parse-seeds from)
                  (col/detect-seeds (vals (:nodes g))))]
    (when (seq seeds)
      (cas/plan-cascade (with-published g registry (graph/downstream-closure g seeds))
                        seeds
                        {:requested-bump (ropts/requested-bump opts)
                         :await (cond-> {}
                                  no-wait (assoc :mode :skip)
                                  await-timeout (assoc :timeout-ms await-timeout))}))))

(defn cascade-cmd
  "Plan a transitive release cascade across the workspace.

   Seeds come from --from <csv>; without it, every project holding unpublished
   commits seeds the cascade. Planning only unless --apply is passed.
   `:registry` (default bb-depsolve.registry.live/live-registry) is the
   IArtifactRegistry the plan reads published versions from.
   With --apply --release-pr, a complete cascade opens a release pull request
   for every released project whose branch is not its default
   (`open-release-prs!`); `:open-pr!` (default `pr/open-release-pr!`) is that
   forge port."
  [{:keys [opts registry open-pr!]}]
  (let [{:keys [root org no-wait await-timeout apply force release-pr]
         out-format :format
         :or {root "."}} opts
        out-format (or out-format "text")
        root-dir (str (fs/canonicalize root))
        registry (or registry (registry/live-registry))]
    (ropts/require-org! org "cascade")
    (if-let [plan (cascade-plan opts registry)]
      (cond
        (= out-format "edn") (pp/pprint plan)

        apply
        (do (render/print-plan plan)
            (println)
            (println (ui/c :bold "Executing — this bumps, tags and pushes."))
            (println)
            (execute-cascade! root-dir plan {:force? force
                                             :no-wait no-wait
                                             :await-timeout await-timeout
                                             :release-pr? release-pr
                                             :open-pr! open-pr!}))

        :else (render/print-plan plan))
      (println (ui/c :green "Nothing to cascade: every project is published up to date.")))))

(defn reconcile-cmd
  "Rewrite VERSION files that lag the versions already evidenced by their own
   git tags or by the pins their consumers carry.

   Reports only unless --apply is passed. A project whose upstream branch
   already declares the value is reported as a pull and never rewritten, and
   a file already at the value is never touched: a no-op VERSION edit blocks
   the next release commit. --commit commits exactly the VERSION files
   written, one commit per project."
  [{:keys [opts]}]
  (let [{:keys [root org skip-dirs depth apply commit]
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
          (let [{:keys [pull rewrite]}
                (reconcile/partition-upstream
                 drifts
                 (into {} (map (fn [{:keys [project]}]
                                 [project (reconcile/upstream-version (dir-of project))]))
                       drifts))]
            ;; Upstream already carries the value: a local rewrite would only
            ;; leave an uncommitted VERSION edit that blocks the release merge.
            (doseq [{:keys [project upstream]} pull]
              (println (ui/c :yellow (format "  %s — upstream already declares %s; pull instead of rewriting"
                                             project upstream))))
            (if apply
              (let [written (into {}
                                  (keep (fn [{:keys [project] :as d}]
                                          (let [paths (reconcile/apply-drift! (dir-of project) d)]
                                            (println (ui/c :green (format "  %s — wrote %d VERSION file(s)"
                                                                          project (count paths))))
                                            ;; commit-paths! keys by the dir under root.
                                            (when (seq paths)
                                              [(str (fs/relativize root-dir (dir-of project))) paths]))))
                                  rewrite)]
                (if commit
                  (git/commit-paths! root-dir written "chore(version): reconcile VERSION with released versions")
                  (when (seq written)
                    (println (ui/c :dim "  Pass --commit to commit these, one commit per project.")))))
              (println (ui/c :dim "  Dry run. Pass --apply to rewrite these VERSION files.")))))))))