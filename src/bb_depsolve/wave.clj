(ns bb-depsolve.wave
  "Workspace-wide release automation commands.

   Layer 3 (Intent/Orchestration): composes per-project commands from
   bb-depsolve.core into workspace-wide waves.

   Commands:
     bump-wave-cmd    — bump all projects with commits ahead of tag
     push-all-cmd     — push all workspace projects to remotes
     release-wave-cmd — full release: upgrade → lint → sync → bump → push
     help-cmd         — print CLI help"
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [bb-depsolve.core :as core]))

(defn bump-wave-cmd
  "Bump all workspace projects that have commits ahead of their last tag."
  [{:keys [opts]}]
  (let [{:keys [root apply skip-dirs org]
         :or {root "."}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   core/default-skip-dirs)
        projects (core/find-workspace-projects root-dir skip-set)]

    (println (core/c :bold (format "Scanning %d projects for version bumps..." (count projects))))
    (println)

    (let [to-bump (atom [])]
      (doseq [project-dir projects
              :let [project (str (fs/file-name project-dir))
                    version (str/trim (slurp (str (fs/path project-dir "VERSION"))))
                    tag (str "v" version)
                    ahead (core/git-commits-ahead (str project-dir) tag)]
              :when (pos? ahead)]
        (swap! to-bump conj {:project project :dir (str project-dir)
                             :version version :ahead ahead})
        (printf "  %-30s %s  (%d commits ahead)\n"
                (core/c :cyan project) (core/c :dim version) ahead))

      (println)

      (if (empty? @to-bump)
        (println (core/c :green "All projects are up to date with their tags."))
        (do
          (println (core/c :yellow (format "%d projects to bump." (count @to-bump))))
          (println)
          (if apply
            (do
              (doseq [{:keys [project dir]} @to-bump]
                (println (core/c :bold (str "Bumping " project "...")))
                (core/bump-cmd {:opts {:root dir :minor true}}))
              (println)
              (when org
                (println (core/c :bold "Re-syncing workspace after bumps..."))
                (core/sync-cmd {:opts {:root root :org org :apply true}})
                (core/auto-commit-workspace! root-dir
                                             (core/find-dep-files {:root root :skip-dirs skip-set})
                                             "chore: sync after bump-wave (bb-depsolve)")))
            (println (core/c :dim "  Dry run. Pass --apply to bump all."))))))))

(defn push-all-cmd
  "Push all workspace projects to their remotes."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs]
         :or {root "."}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   core/default-skip-dirs)
        projects (->> (fs/list-dir root-dir)
                      (filter fs/directory?)
                      (remove #(core/skip-path? root-dir skip-set %))
                      (filter #(fs/exists? (fs/path % ".git")))
                      (sort))]

    (println (core/c :bold (format "Pushing %d projects..." (count projects))))
    (println)

    (let [pushed (atom 0)
          skipped (atom 0)
          failed (atom 0)]
      (doseq [project-dir projects
              :let [project (str (fs/file-name project-dir))
                    dir (str project-dir)]]
        (if-not (core/git-has-remote? dir)
          (do (swap! skipped inc)
              (println (core/c :dim (str "  " project " — no remote, skipped"))))
          (let [push-result (core/git dir "push")
                tag-result (core/git dir "push" "--tags")]
            (if (and (zero? (:exit push-result)) (zero? (:exit tag-result)))
              (do (swap! pushed inc)
                  (println (core/c :green (str "  " project " — pushed"))))
              (do (swap! failed inc)
                  (println (core/c :yellow (str "  " project " — push failed"))))))))

      (println)
      (println (core/c :bold (format "Pushed: %d  Skipped: %d  Failed: %d"
                                     @pushed @skipped @failed))))))

(defn release-wave-cmd
  "Full workspace release: upgrade → lint → sync → bump → re-sync → push."
  [{:keys [opts]}]
  (let [{:keys [root org apply skip-dirs depth]
         :or {root "." depth core/default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   core/default-skip-dirs)]

    (when-not org
      (println (core/c :red "Error: --org is required for release-wave"))
      (System/exit 1))

    (when-not apply
      (println (core/c :bold "Release wave dry-run — pass --apply to execute"))
      (println)
      (println "  Phase 1: upgrade    — fetch latest external deps")
      (println "  Phase 2: lint --fix — resolve :local/root to git tags")
      (println "  Phase 3: sync       — align internal git deps to latest tags")
      (println "  Phase 4: bump-wave  — bump all projects with commits ahead")
      (println "  Phase 5: re-sync    — propagate new tags from bumps")
      (println "  Phase 6: push-all   — push everything to remotes")
      (println)
      (println (core/c :dim "Pass --apply to execute the full release wave."))
      (System/exit 0))

    (let [base-opts {:root root :org org :skip-dirs skip-dirs :depth depth}]

      ;; Phase 1: Upgrade external deps
      (println (core/c :bold "═══ Phase 1: Upgrading external deps ═══"))
      (println)
      (core/upgrade-cmd {:opts (assoc base-opts :apply true :commit true)})
      (println)

      ;; Phase 2: Lint + fix
      (println (core/c :bold "═══ Phase 2: Fixing :local/root anti-patterns ═══"))
      (println)
      (core/lint-cmd {:opts (assoc base-opts :fix true)})
      (core/auto-commit-workspace! root-dir
                                   (core/find-dep-files {:root root :skip-dirs skip-set :depth depth})
                                   "fix: resolve :local/root deps as git tags (bb-depsolve)")
      (println)

      ;; Phase 3: Sync internal deps
      (println (core/c :bold "═══ Phase 3: Syncing internal git deps ═══"))
      (println)
      (core/sync-cmd {:opts (assoc base-opts :apply true :commit true)})
      (println)

      ;; Phase 4: Bump wave
      (println (core/c :bold "═══ Phase 4: Bumping all ahead projects ═══"))
      (println)
      (bump-wave-cmd {:opts (assoc base-opts :apply true)})
      (println)

      ;; Phase 5: Re-sync (bumps created new tags)
      (println (core/c :bold "═══ Phase 5: Re-syncing after bumps ═══"))
      (println)
      (core/sync-cmd {:opts (assoc base-opts :apply true :commit true)})
      (println)

      ;; Phase 6: Push
      (println (core/c :bold "═══ Phase 6: Pushing all to remotes ═══"))
      (println)
      (push-all-cmd {:opts base-opts})
      (println)

      (println (core/c :green (core/c :bold "Release wave complete."))))))

(defn help-cmd
  "Print help text for available commands."
  [dispatch-table & _]
  (println (core/c :bold "bb-depsolve") " — monorepo dependency management")
  (println)
  (println "Usage: bb-depsolve <command> [options]")
  (println)
  (println (core/c :bold "Commands:"))
  (doseq [{:keys [cmds doc]} dispatch-table
          :when (seq cmds)]
    (printf "  %-12s %s\n" (str/join " " cmds) doc))
  (println)
  (println (core/c :bold "Common options:"))
  (println "  --root <dir>       Workspace root (default: cwd)")
  (println "  --skip-dirs <csv>  Directories to skip (default: vendor,node_modules,.git,target,...)")
  (println "  --depth <n>        How deep to scan for dep files (default: 1)")
  (println "  --apply            Write changes (default: dry-run)")
  (println)
  (println (core/c :bold "Sync options:"))
  (println "  --org <name>       GitHub org for internal deps (required for sync)")
  (println)
  (println (core/c :bold "Upgrade options:"))
  (println "  --pre-release      Include pre-release versions")
  (println)
  (println (core/c :bold "Lint options:"))
  (println "  --fix              Auto-fix: split :local/root into local.deps.edn")
  (println "  --org <name>       GitHub org for resolving internal deps (used with --fix)")
  (println)
  (println (core/c :bold "Tree options:"))
  (println "  --tree-depth <n>   Max transitive depth (default: full resolve)")
  (println "  --conflicts-only   Show only deps with version conflicts")
  (println)
  (println (core/c :bold "Audit options:"))
  (println "  --tree-depth <n>   Include transitive deps (default: direct only)")
  (println)
  (println (core/c :bold "Automation options:"))
  (println "  --commit           Auto-commit dep changes per project (for upgrade/sync/lint)")
  (println)
  (println (core/c :bold "Release-wave:"))
  (println "  --org <name>       GitHub org (required)")
  (println "  Runs: upgrade → lint --fix → sync → bump-wave → re-sync → push-all"))
