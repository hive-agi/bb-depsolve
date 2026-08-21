(ns bb-depsolve.wave.commands
  "Workspace-wide release orchestration: bump-wave, push-all, release-wave."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.core.bump :as bump]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.core.lint :as lint]
            [bb-depsolve.core.sync :as sync]
            [bb-depsolve.core.upgrade :as upgrade]
            [bb-depsolve.core.push :as push]))

(defn bump-wave-cmd
  "Bump all workspace projects that have commits ahead of their last tag."
  [{:keys [opts]}]
  (let [{:keys [root apply skip-dirs org]
         :or {root "."}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        projects (discovery/find-workspace-projects root-dir skip-set)]

    (println (ui/c :bold (format "Scanning %d projects for version bumps..." (count projects))))
    (println)

    (let [to-bump (atom [])]
      (doseq [project-dir projects
              :let [project (str (fs/file-name project-dir))
                    version (str/trim (slurp (str (fs/path project-dir "VERSION"))))
                    tag (str "v" version)
                    ahead (git/git-commits-ahead (str project-dir) tag)]
              :when (pos? ahead)]
        (swap! to-bump conj {:project project :dir (str project-dir)
                             :version version :ahead ahead})
        (printf "  %-30s %s  (%d commits ahead)\n"
                (ui/c :cyan project) (ui/c :dim version) ahead))

      (println)

      (if (empty? @to-bump)
        (println (ui/c :green "All projects are up to date with their tags."))
        (do
          (println (ui/c :yellow (format "%d projects to bump." (count @to-bump))))
          (println)
          (if apply
            (do
              (doseq [{:keys [project dir]} @to-bump]
                (println (ui/c :bold (str "Bumping " project "...")))
                (bump/bump-cmd {:opts {:root dir :apply true}}))
              (println)
              (when org
                (println (ui/c :bold "Re-syncing workspace after bumps..."))
                (sync/sync-cmd {:opts {:root root :org org :apply true}})
                (git/auto-commit-workspace! root-dir
                                             (discovery/find-dep-files {:root root :skip-dirs skip-set})
                                             "chore: sync after bump-wave (bb-depsolve)")))
            (println (ui/c :dim "  Dry run. Pass --apply to bump all."))))))))

(defn push-all-cmd
  "Push all workspace projects to their remotes.

   --sync     fetch first and MERGE an upstream that has moved ahead (a CI
              release commit, typically) instead of failing non-fast-forward.
              A project whose working tree has modifications to a file the
              incoming commits touch is skipped: that tree belongs to whoever
              dirtied it.
   --resolve  settle a conflicted merge with the sweep's policy
              (deps.edn/bb.edn -> ours, VERSION -> theirs, build.clj -> drop).
              Any other conflicted path aborts the merge untouched."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs sync resolve]
         :or {root "."}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        projects (->> (fs/list-dir root-dir)
                      (filter fs/directory?)
                      (remove #(discovery/skip-path? root-dir skip-set %))
                      (filter #(fs/exists? (fs/path % ".git")))
                      (sort))
        push! (fn [dir]
                (let [p (git/git dir "push")
                      t (git/git dir "push" "--tags")]
                  (and (zero? (:exit p)) (zero? (:exit t)))))
        settle! (fn [dir conflicts]
                  (let [{:keys [resolvable? actions unknown]} (push/conflict-plan conflicts)]
                    (if-not resolvable?
                      (do (git/git dir "merge" "--abort") {:ok? false :unknown unknown})
                      (do (doseq [[path action] actions]
                            (case action
                              :ours (do (git/git dir "checkout" "--ours" path)
                                        (git/git dir "add" path))
                              :theirs (do (git/git dir "checkout" "--theirs" path)
                                          (git/git dir "add" path))
                              :remove (git/git dir "rm" "-q" path)
                              nil))
                          (let [c (git/git dir "commit" "--no-edit")]
                            {:ok? (zero? (:exit c))})))))]

    (println (ui/c :bold (format "Pushing %d projects..." (count projects))))
    (println)

    (let [tally (atom {})
          bump! (fn [k] (swap! tally update k (fnil inc 0)))]
      (doseq [project-dir projects
              :let [project (str (fs/file-name project-dir))
                    dir (str project-dir)
                    _ (when sync (git/git dir "fetch" "--quiet"))
                    upstream (git/git-upstream dir)
                    {:keys [ahead behind]} (if upstream
                                             (git/git-ahead-behind dir upstream)
                                             {:ahead 0 :behind 0})
                    plan (push/push-plan
                          {:has-remote? (git/git-has-remote? dir)
                           :upstream upstream
                           :ahead ahead
                           :behind behind
                           :dirty-files (git/git-dirty-files dir)
                           :incoming-files (when upstream (git/git-incoming-files dir upstream))})]]
        (case (:action plan)
          :skip
          (do (bump! :skipped)
              (println (ui/c :dim (format "  %-24s skipped — %s%s" project
                                          (name (:reason plan))
                                          (if-let [f (:files plan)]
                                            (str " (" (str/join ", " f) ")")
                                            "")))))
          :push
          (if (push! dir)
            (do (bump! :pushed) (println (ui/c :green (str "  " project " — pushed"))))
            (do (bump! :failed) (println (ui/c :yellow (str "  " project " — push failed")))))

          :merge-then-push
          (if-not sync
            (do (bump! :skipped)
                (println (ui/c :dim (format "  %-24s skipped — %d behind, pass --sync" project behind))))
            (let [m (git/git dir "merge" "--no-edit" upstream)
                  conflicts (when-not (zero? (:exit m)) (git/git-conflicted-files dir))
                  settled (cond
                            (zero? (:exit m)) {:ok? true}
                            (not resolve) (do (git/git dir "merge" "--abort")
                                              {:ok? false :unknown conflicts})
                            :else (settle! dir conflicts))]
              (cond
                (not (:ok? settled))
                (do (bump! :conflicted)
                    (println (ui/c :yellow (format "  %-24s merge conflict — %s" project
                                                   (str/join ", " (:unknown settled))))))
                (push! dir)
                (do (bump! :pushed) (println (ui/c :green (str "  " project " — merged + pushed"))))
                :else
                (do (bump! :failed) (println (ui/c :yellow (str "  " project " — push failed")))))))))

      (println)
      (println (ui/c :bold (format "Pushed: %d  Skipped: %d  Conflicted: %d  Failed: %d"
                                   (get @tally :pushed 0) (get @tally :skipped 0)
                                   (get @tally :conflicted 0) (get @tally :failed 0)))))))

(defn release-wave-cmd
  "Full workspace release: upgrade → lint → sync → bump → re-sync → push."
  [{:keys [opts]}]
  (let [{:keys [root org apply skip-dirs depth]
         :or {root "." depth discovery/default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)]

    (when-not org
      (println (ui/c :red "Error: --org is required for release-wave"))
      (System/exit 1))

    (when-not apply
      (println (ui/c :bold "Release wave dry-run — pass --apply to execute"))
      (println)
      (println "  Phase 1: upgrade    — fetch latest external deps")
      (println "  Phase 2: lint --fix — resolve :local/root to git tags")
      (println "  Phase 3: sync       — align internal Git tags and published Maven versions")
      (println "  Phase 4: bump-wave  — bump all projects with commits ahead")
      (println "  Phase 5: re-sync    — propagate new tags from bumps")
      (println "  Phase 6: push-all   — push everything to remotes")
      (println)
      (println (ui/c :dim "Pass --apply to execute the full release wave."))
      (System/exit 0))

    (let [base-opts {:root root :org org :skip-dirs skip-dirs :depth depth}]

      ;; Phase 1: Upgrade external deps
      (println (ui/c :bold "═══ Phase 1: Upgrading external deps ═══"))
      (println)
      (upgrade/upgrade-cmd {:opts (assoc base-opts :apply true :commit true)})
      (println)

      ;; Phase 2: Lint + fix
      (println (ui/c :bold "═══ Phase 2: Fixing :local/root anti-patterns ═══"))
      (println)
      (lint/lint-cmd {:opts (assoc base-opts :fix true)})
      (git/auto-commit-workspace! root-dir
                                   (discovery/find-dep-files {:root root :skip-dirs skip-set :depth depth})
                                   "fix: resolve :local/root deps as git tags (bb-depsolve)")
      (println)

      ;; Phase 3: Sync internal deps
      (println (ui/c :bold "═══ Phase 3: Syncing internal Git and Maven deps ═══"))
      (println)
      (sync/sync-cmd {:opts (assoc base-opts :apply true :commit true)})
      (println)

      ;; Phase 4: Bump wave
      (println (ui/c :bold "═══ Phase 4: Bumping all ahead projects ═══"))
      (println)
      (bump-wave-cmd {:opts (assoc base-opts :apply true)})
      (println)

      ;; Phase 5: Re-sync (bumps created new tags)
      (println (ui/c :bold "═══ Phase 5: Re-syncing after bumps ═══"))
      (println)
      (sync/sync-cmd {:opts (assoc base-opts :apply true :commit true)})
      (println)

      ;; Phase 6: Push
      (println (ui/c :bold "═══ Phase 6: Pushing all to remotes ═══"))
      (println)
      (push-all-cmd {:opts base-opts})
      (println)

      (println (ui/c :green (ui/c :bold "Release wave complete."))))))
