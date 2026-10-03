(ns bb-depsolve.core.bump
  "The bump command: VERSION bump, tag, push."
  (:require [babashka.fs :as fs]
            [babashka.process :as proc]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.sync :as sync]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.version.api :as v]
            [clojure.string :as str]
            [hive-dsl.result :as r]))

(defn find-consumers
  "Scan workspace for projects whose dep files reference TARGET-LIB.
   Returns vec of {:project :path :version}. Pure-ish (slurps files).
   Used by major-bump compatibility warning."
  [root-dir skip-set target-lib]
  (let [target-str (str target-lib)
        dep-files (discovery/find-dep-files {:root root-dir :skip-dirs skip-set})]
    (->> dep-files
         (keep (fn [{:keys [path project] :as df}]
                 (let [content (slurp path)
                       git-hit (->> (v/find-git-deps content)
                                    (filter #(= target-str (str (:lib %))))
                                    first)
                       mvn (->> (discovery/extract-mvn-deps df content)
                                (filter #(= target-str (str (:lib %))))
                                first)]
                   (when-let [hit (or git-hit mvn)]
                     {:project project
                      :path path
                      :version (or (:tag hit) (:version hit))}))))
         (vec))))

(defn warn-major-bump!
  "Warn the user before performing a major version bump, listing workspace
   consumers that would need a coordinated update.
   Returns true to proceed, false to abort."
  [project-dir lib-sym old-tag new-tag]
  (let [root (str (fs/parent project-dir))
        consumers (find-consumers root discovery/default-skip-dirs lib-sym)]
    (println (ui/c :yellow (format "MAJOR BUMP: %s %s -> %s" lib-sym old-tag new-tag)))
    (if (empty? consumers)
      (do (println (ui/c :dim "  No workspace consumers found. Proceeding."))
          true)
      (do
        (println (ui/c :yellow (format "  %d workspace consumer(s) depend on %s:"
                                    (count consumers) lib-sym)))
        (doseq [{:keys [project version]} consumers]
          (println (str "    " (ui/c :cyan project) " @ " (ui/c :dim version))))
        (println (ui/c :dim "  Run `bb-depsolve sync --apply` after bump to align."))
        true))))

(defn bump-level
  "Semver segment the CLI flags ask for. --major and --stable raise the major
   segment, --minor the minor, everything else the patch. --stable is a major
   promotion: on a pre-1.0 project that is exactly the 1.0.0 release. The
   strongest flag present wins. Pure."
  [{:keys [major minor stable]}]
  (cond (or major stable) :major
        minor             :minor
        :else             :patch))

(defn plan-bump
  "The bump CURRENT-VERSION would receive under OPTS, as a plan value:
   {:current :level :new-version :new-tag}. Returns nil when CURRENT-VERSION
   cannot be parsed. Pure — decides, writes nothing."
  [current-version opts]
  (when-let [current (v/parse-semver current-version)]
    (let [level (bump-level opts)
          next-semver ((case level
                         :major v/bump-major
                         :minor v/bump-minor
                         :patch v/bump-patch)
                       current)]
      {:current     current-version
       :level       level
       :new-version (v/semver->version next-semver)
       :new-tag     (v/semver->tag next-semver)})))

(defn- git-step
  "Run one git ARGV through the GIT port; a non-zero exit is an error of TYPE
   carrying the argv and stderr."
  [git argv type]
  (let [{:keys [exit err]} (apply git argv)]
    (if (zero? exit)
      (r/ok argv)
      (r/err {:type type :argv argv :stderr (str/trim (or err ""))}))))

(defn publish-release!
  "Commit the staged release, tag it TAG, push the branch and only then the tag.

   GIT is the port: (fn [& argv] -> {:exit :out :err}). The tag push is
   conditional on the branch push: on a protected branch (GH006, PR-required)
   the branch push is rejected while `git push --tags` still succeeds, which
   would publish a tag one commit ahead of the remote branch, orphaned, and the
   next bump would collide on a stale VERSION. So a rejected branch push stops
   here with :bump/branch-push-rejected and the tag stays local.

   => (r/ok {:tag :pushed #{:branch :tag}}) or (r/err {:type ...})."
  [git tag]
  (r/let-ok [_ (git-step git ["commit" "-m" (str "release: " tag)] :bump/git-failed)
             _ (git-step git ["tag" tag] :bump/git-failed)
             _ (git-step git ["push"] :bump/branch-push-rejected)
             _ (git-step git ["push" "--tags"] :bump/tag-push-failed)]
    (r/ok {:tag tag :pushed #{:branch :tag}})))

(defn- report-publish!
  "Print the outcome of `publish-release!`. Returns true when it landed."
  [tag res]
  (if (r/ok? res)
    (do (println (ui/c :green (str "  Committed, tagged and pushed " tag)))
        true)
    (let [{:keys [type stderr]} (:error res)]
      (case type
        :bump/branch-push-rejected
        (do (println (ui/c :red (str "  Branch push rejected; tag " tag " NOT pushed.")))
            (when-not (str/blank? stderr) (println (ui/c :dim (str "  " stderr))))
            (println (ui/c :yellow (str "  The release commit and tag " tag " exist only locally. "
                                        "If the branch is protected, push a release branch, open a PR, "
                                        "merge it with a merge commit, then `git push origin " tag "`."))))
        :bump/tag-push-failed
        (println (ui/c :red (str "  Branch pushed but tag push failed: " stderr)))
        (println (ui/c :red (str "  git failed: " stderr))))
      false)))

(defn bump-cmd
  "Bump the VERSION file, git commit + tag + push, optionally sync downstream.
   Writes nothing without --apply: the default is a dry run that prints the
   planned bump. A major increment beyond v0 first lists the workspace
   consumers that would need a coordinated update, and --force is required to
   proceed past that warning.

   The tag is pushed only after the branch push lands (`publish-release!`): a
   protected branch that rejects the release commit must not receive an
   orphaned tag. A rejected push exits 1 and skips --sync. `:exit!` (default
   System/exit) is the process-exit port."
  [{:keys [opts exit!] :or {exit! #(System/exit %)}}]
  (let [{:keys [root sync org force apply]
         :or {root "."}} opts
        project-dir (str (fs/canonicalize root))
        version-file (str (fs/path project-dir "VERSION"))]

    (if-not (fs/exists? version-file)
      (do (println (ui/c :red (str "Error: VERSION file not found at " version-file)))
          (exit! 1))

    (let [current-str (str/trim (slurp version-file))
          plan        (plan-bump current-str opts)]

      (if-not plan
        (do (println (ui/c :red (str "Error: Cannot parse version '" current-str "'")))
            (exit! 1))

      (let [{:keys [level new-version new-tag]} plan
            project-name (str (fs/file-name project-dir))
            workspace    (str (fs/parent project-dir))
            ;; Best-effort consumer scan keyed by artifact-id (project-name).
            consumers    (when (v/major-bump? (str "v" current-str) new-tag)
                           (->> (discovery/find-dep-files
                                 {:root workspace
                                  :skip-dirs discovery/default-skip-dirs})
                                (keep (fn [{:keys [path project] :as df}]
                                        (let [content (slurp path)
                                              hits (concat (v/find-git-deps content)
                                                           (discovery/extract-mvn-deps df content))
                                              match (some #(when (= project-name
                                                                    (v/lib-artifact-id (:lib %)))
                                                             %) hits)]
                                          (when match
                                            {:project project
                                             :version (or (:tag match) (:version match))}))))
                                (vec)))]

        (when (seq consumers)
          (println (ui/c :yellow (format "MAJOR BUMP: %s -> %s" current-str new-version)))
          (println (ui/c :yellow (format "  %d workspace consumer(s) reference '%s':"
                                         (count consumers) project-name)))
          (doseq [{:keys [project version]} consumers]
            (println (str "    " (ui/c :cyan project) " @ " (ui/c :dim version))))
          (println (ui/c :dim "  Run `bb-depsolve sync --apply` after bump to align.")))

        (if (and (seq consumers) (not force))
          (do (println (ui/c :dim "  Pass --force to bypass this warning."))
              (exit! 1))
          (do
            (println (ui/c :bold (format "Bumping %s -> %s (%s)" current-str new-version (name level))))
            (println)

            (if-not apply
              (println (ui/c :dim "  Dry run. Pass --apply to write VERSION, tag and push."))
              (do
                (spit version-file (str new-version "\n"))
                (println (ui/c :green (str "  Updated VERSION: " new-version)))

                (let [extra-version-files (->> (fs/glob project-dir "**/VERSION")
                                               (map str)
                                               (remove #{version-file}))]
                  (doseq [f extra-version-files]
                    (spit f (str new-version "\n"))
                    (println (ui/c :green (str "  Updated " (fs/relativize project-dir f))))))

                (let [run-git (fn [& args]
                                (proc/sh (into ["git" "-C" project-dir] args)))
                      all-version-files (into ["VERSION"]
                                              (->> (fs/glob project-dir "**/VERSION")
                                                   (map #(str (fs/relativize project-dir %)))))]
                  (doseq [f all-version-files]
                    (run-git "add" f))
                  (if-not (report-publish! new-tag (publish-release! run-git new-tag))
                    (exit! 1)
                    (do
                      (println)
                      (when (and sync org)
                        (println (ui/c :bold "Running sync..."))
                        (sync/sync-cmd {:opts {:root (str (fs/parent project-dir))
                                               :org org :apply true}}))
                      (println (ui/c :green (str "Done: " new-tag))))))))))))))))