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
            [bb-depsolve.core :as core]
            [hive-dsl.bounded-atom :as ba]
            [bb-depsolve.version :as v]
            [clojure.pprint :as pp]))

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

(def ^:private default-gitignore-entries
  ["target/"
   ".cpcache/"
   ".nrepl-port"
   ".lsp/"
   ".clj-kondo/.cache/"
   "local.deps.edn"
   "*.iml"
   ".idea/"
   ".DS_Store"
   "*.log"])

(defn- ensure-gitignore-lines!
  "Idempotently append missing entries to .gitignore. Returns count added.
   Preserves existing content; only appends when entries are missing."
  [project-dir entries]
  (let [gi (str (fs/path project-dir ".gitignore"))
        content (if (fs/exists? gi) (slurp gi) "")
        present (->> (str/split-lines content)
                     (map str/trim)
                     (set))
        missing (remove present entries)]
    (when (seq missing)
      (let [trailing (if (or (empty? content)
                             (str/ends-with? content "\n"))
                       ""
                       "\n")
            block (str trailing
                       (when (or (empty? content)
                                 (not (str/blank? (last (str/split-lines content)))))
                         (when (seq content) "\n"))
                       "# bb-depsolve auto-added\n"
                       (str/join "\n" missing)
                       "\n")]
        (spit gi (str content block))))
    (count missing)))

(defn gitignore-cmd
  "Add common Clojure entries to each workspace project's .gitignore.
   Idempotent: never duplicates entries.

   Default entries: target/ .cpcache/ .nrepl-port .lsp/ .clj-kondo/.cache/
                    local.deps.edn *.iml .idea/ .DS_Store *.log

   Pass --extra <csv> to add custom entries beyond defaults."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs extra]
         :or {root "."}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   core/default-skip-dirs)
        extra-set (when extra (str/split extra #","))
        entries (vec (distinct (concat default-gitignore-entries extra-set)))
        projects (->> (fs/list-dir root-dir)
                      (filter fs/directory?)
                      (remove #(core/skip-path? root-dir skip-set %))
                      (filter #(fs/exists? (fs/path % ".git")))
                      (sort))]

    (println (core/c :bold (format "Updating .gitignore in %d projects..." (count projects))))
    (println)

    (let [updated (atom 0)
          unchanged (atom 0)]
      (doseq [project-dir projects
              :let [project (str (fs/file-name project-dir))
                    added (ensure-gitignore-lines! (str project-dir) entries)]]
        (if (pos? added)
          (do (swap! updated inc)
              (println (core/c :green (format "  %s — added %d entr%s"
                                               project added
                                               (if (= 1 added) "y" "ies")))))
          (do (swap! unchanged inc)
              (println (core/c :dim (str "  " project " — already up to date"))))))

      (println)
      (println (core/c :bold (format "Updated: %d  Unchanged: %d" @updated @unchanged))))))

(defn rename-branch-cmd
  "Rename the default branch (default master->main) in each workspace project.
   Steps per project:
     1. git branch -m <from> <to>          (local rename)
     2. git push -u origin <to>            (push new branch)
     3. git symbolic-ref refs/remotes/origin/HEAD refs/remotes/origin/<to>
     4. git push origin --delete <from>    (remove old branch on remote)

   Skips projects where:
     - the FROM branch does not exist locally
     - the TO branch already exists
     - --apply is not set (dry-run)"
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs from to apply skip-remote]
         :or {root "." from "master" to "main"}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   core/default-skip-dirs)
        projects (->> (fs/list-dir root-dir)
                      (filter fs/directory?)
                      (remove #(core/skip-path? root-dir skip-set %))
                      (filter #(fs/exists? (fs/path % ".git")))
                      (sort))]

    (println (core/c :bold (format "Renaming branch '%s' -> '%s' across %d projects..."
                                    from to (count projects))))
    (println)

    (let [renamed (atom 0)
          skipped (atom 0)]
      (doseq [project-dir projects
              :let [project (str (fs/file-name project-dir))
                    dir (str project-dir)
                    branches (core/git dir "branch" "--list")
                    has-from? (str/includes? (or (:out branches) "") from)
                    has-to?   (str/includes? (or (:out branches) "") to)]]
        (cond
          (not has-from?)
          (do (swap! skipped inc)
              (println (core/c :dim (format "  %s — no '%s' branch, skipped" project from))))

          has-to?
          (do (swap! skipped inc)
              (println (core/c :dim (format "  %s — '%s' already exists, skipped" project to))))

          (not apply)
          (println (core/c :yellow (format "  %s — would rename '%s' -> '%s'" project from to)))

          :else
          (let [r1 (core/git dir "branch" "-m" from to)]
            (if (zero? (:exit r1))
              (do (swap! renamed inc)
                  (println (core/c :green (format "  %s — renamed locally" project)))
                  (when-not skip-remote
                    (let [r2 (core/git dir "push" "-u" "origin" to)
                          r3 (core/git dir "push" "origin" "--delete" from)]
                      (when (zero? (:exit r2))
                        (println (core/c :green (str "    pushed '" to "' to origin"))))
                      (when (zero? (:exit r3))
                        (println (core/c :green (str "    deleted '" from "' on origin")))))))
              (do (swap! skipped inc)
                  (println (core/c :yellow (format "  %s — local rename failed (%s)"
                                                   project (str/trim (or (:err r1) ""))))))))))

      (println)
      (println (core/c :bold (format "Renamed: %d  Skipped: %d" @renamed @skipped)))
      (when-not apply
        (println (core/c :dim "  Dry run. Pass --apply to perform the rename."))))))

(defn lock-cmd
  "Generate deps.lock.edn for each workspace project containing the
   resolved transitive dependency tree (Maven nearest-wins).

   Output format (per project):
     {:lock-version 1
      :generated-at <iso-timestamp>
      :source <relative-path-to-deps-file>
      :resolved {<lib> {:version <v> :type <:mvn|:git> :depth <n>}}
      :conflicts {<lib> [<v> ...]}}

   Locks are deterministic given the same inputs. Re-run after upgrade/sync."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs depth tree-depth]
         :or {root "." depth core/default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   core/default-skip-dirs)
        dep-files (core/find-dep-files {:root root :skip-dirs skip-set :depth depth})
        cache (ba/bounded-atom {:max-entries 500})]

    (println (core/c :bold (format "Generating deps.lock.edn for %d dep files..."
                                    (count dep-files))))
    (println)

    (let [locked (atom 0)]
      (doseq [{:keys [path project] :as df} dep-files
              :let [content (slurp path)
                    mvn-deps (core/extract-mvn-deps df content)
                    git-deps (if (core/shadow-deps-file? df)
                               []
                               (mapv (fn [{:keys [lib tag]}]
                                       {:lib lib :version tag :type :git})
                                     (v/find-git-deps content)))
                    direct (vec (concat
                                 (mapv (fn [{:keys [lib version]}]
                                         {:lib lib :version version :type :mvn})
                                       mvn-deps)
                                 git-deps))
                    resolve-fn (fn [lib version]
                                 (core/resolve-dep-children cache lib version))
                    tree (v/build-dep-tree direct resolve-fn tree-depth)
                    resolution (v/resolve-versions tree)
                    lock-data {:lock-version 1
                               :generated-at (str (java.time.Instant/now))
                               :source (str (fs/relativize root-dir path))
                               :resolved (into (sorted-map)
                                                (for [[lib m] (:resolved resolution)]
                                                  [lib (select-keys m [:version :type :depth])]))
                               :conflicts (into (sorted-map)
                                                 (for [[lib vs] (:conflicts resolution)]
                                                   [lib (vec (sort vs))]))}
                    project-dir (fs/parent path)
                    lock-path (str (fs/path project-dir "deps.lock.edn"))]]
        (spit lock-path (with-out-str (pp/pprint lock-data)))
        (swap! locked inc)
        (println (core/c :green (format "  %s -> %s"
                                         project
                                         (str (fs/relativize root-dir lock-path))))))

      (println)
      (println (core/c :bold (format "Locked: %d project(s)" @locked))))))

(defn deep-lint-cmd
  "Deep lint: fetch the latest published tag's deps.edn from the forge
   and check if it still contains :local/root anti-patterns.

   Catches the case where a project was tagged BEFORE bb-depsolve lint --fix
   ran, so consumers still pull a release pinned to local paths.

   Workspace-level: scans every project with a VERSION + tag remote.
   Reports lib + tag + locals found. Exits non-zero when any issues found."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs org]
         :or {root "."}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   core/default-skip-dirs)
        projects (core/find-workspace-projects root-dir skip-set)]

    (when-not org
      (println (core/c :red "Error: --org is required for deep-lint"))
      (System/exit 1))

    (println (core/c :bold "Deep-lint: scanning latest tagged releases for :local/root..."))
    (println)

    (let [issues (atom 0)
          checked (atom 0)]
      (doseq [project-dir projects
              :let [project (str (fs/file-name project-dir))
                    lib-sym (symbol (str "io.github." org "/" project))
                    tag-r (core/resolve-lib-tags root-dir lib-sym project)]]
        (cond
          (not (and (map? tag-r) (contains? tag-r :ok)))
          (println (core/c :dim (str "  " project " — no resolvable tag, skipped")))

          :else
          (let [{:keys [tag]} (:ok tag-r)
                forge-info (v/parse-forge-lib lib-sym)]
            (if-not forge-info
              (println (core/c :dim (str "  " project " — non-forge lib, skipped")))
              (let [fetched (core/fetch-git-deps-edn (:forge forge-info)
                                                    (:org forge-info)
                                                    (:repo forge-info)
                                                    tag)]
                (swap! checked inc)
                (if-not (and (map? fetched) (contains? fetched :ok))
                  (println (core/c :dim (str "  " project " — could not fetch deps.edn for " tag)))
                  (let [content (:ok fetched)
                        locals (v/find-local-deps content)]
                    (if (empty? locals)
                      (println (core/c :green (format "  %s @ %s — clean" project tag)))
                      (do
                        (swap! issues inc)
                        (println (core/c :yellow (format "  %s @ %s — %d :local/root dep(s)"
                                                          project tag (count locals))))
                        (doseq [{:keys [lib path]} locals]
                          (println (str "      " (core/c :cyan (str lib))
                                        " -> " (core/c :yellow path)))))))))))))

      (println)
      (println (core/c :bold (format "Checked: %d  Issues: %d" @checked @issues)))
      (when (pos? @issues)
        (System/exit 1)))))

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
    (printf "  %-14s %s\n" (str/join " " cmds) doc))
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
  (println (core/c :bold "Graph / impact / cascade:"))
  (println "  --org <name>       GitHub org for internal deps (required)")
  (println "  --lib <project>    Target project (impact)")
  (println "  --from <csv>       Seed projects (cascade; default: everything unpublished)")
  (println "  --format <fmt>     text (default) | edn | dot (graph only)")
  (println "  --no-wait          Plan without waiting for each wave to publish")
  (println "  --await-timeout <ms>  Per-wave publish-wait ceiling (default: 900000)")
  (println "  --major / --stable    Bump the seed's minor / major segment")
  (println)
  (println (core/c :bold "Automation options:"))
  (println "  --commit           Auto-commit dep changes per project (for upgrade/sync/lint)")
  (println)
  (println (core/c :bold "Release-wave:"))
  (println "  --org <name>       GitHub org (required)")
  (println "  Runs: upgrade → lint --fix → sync → bump-wave → re-sync → push-all"))