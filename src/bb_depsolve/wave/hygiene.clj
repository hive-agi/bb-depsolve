(ns bb-depsolve.wave.hygiene
  "Workspace repo hygiene: .gitignore entries and default-branch rename."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.git :as git]))

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
                   discovery/default-skip-dirs)
        extra-set (when extra (str/split extra #","))
        entries (vec (distinct (concat default-gitignore-entries extra-set)))
        projects (->> (fs/list-dir root-dir)
                      (filter fs/directory?)
                      (remove #(discovery/skip-path? root-dir skip-set %))
                      (filter #(fs/exists? (fs/path % ".git")))
                      (sort))]

    (println (ui/c :bold (format "Updating .gitignore in %d projects..." (count projects))))
    (println)

    (let [updated (atom 0)
          unchanged (atom 0)]
      (doseq [project-dir projects
              :let [project (str (fs/file-name project-dir))
                    added (ensure-gitignore-lines! (str project-dir) entries)]]
        (if (pos? added)
          (do (swap! updated inc)
              (println (ui/c :green (format "  %s — added %d entr%s"
                                               project added
                                               (if (= 1 added) "y" "ies")))))
          (do (swap! unchanged inc)
              (println (ui/c :dim (str "  " project " — already up to date"))))))

      (println)
      (println (ui/c :bold (format "Updated: %d  Unchanged: %d" @updated @unchanged))))))

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
                   discovery/default-skip-dirs)
        projects (->> (fs/list-dir root-dir)
                      (filter fs/directory?)
                      (remove #(discovery/skip-path? root-dir skip-set %))
                      (filter #(fs/exists? (fs/path % ".git")))
                      (sort))]

    (println (ui/c :bold (format "Renaming branch '%s' -> '%s' across %d projects..."
                                    from to (count projects))))
    (println)

    (let [renamed (atom 0)
          skipped (atom 0)]
      (doseq [project-dir projects
              :let [project (str (fs/file-name project-dir))
                    dir (str project-dir)
                    branches (git/git dir "branch" "--list")
                    has-from? (str/includes? (or (:out branches) "") from)
                    has-to?   (str/includes? (or (:out branches) "") to)]]
        (cond
          (not has-from?)
          (do (swap! skipped inc)
              (println (ui/c :dim (format "  %s — no '%s' branch, skipped" project from))))

          has-to?
          (do (swap! skipped inc)
              (println (ui/c :dim (format "  %s — '%s' already exists, skipped" project to))))

          (not apply)
          (println (ui/c :yellow (format "  %s — would rename '%s' -> '%s'" project from to)))

          :else
          (let [r1 (git/git dir "branch" "-m" from to)]
            (if (zero? (:exit r1))
              (do (swap! renamed inc)
                  (println (ui/c :green (format "  %s — renamed locally" project)))
                  (when-not skip-remote
                    (let [r2 (git/git dir "push" "-u" "origin" to)
                          r3 (git/git dir "push" "origin" "--delete" from)]
                      (when (zero? (:exit r2))
                        (println (ui/c :green (str "    pushed '" to "' to origin"))))
                      (when (zero? (:exit r3))
                        (println (ui/c :green (str "    deleted '" from "' on origin")))))))
              (do (swap! skipped inc)
                  (println (ui/c :yellow (format "  %s — local rename failed (%s)"
                                                   project (str/trim (or (:err r1) ""))))))))))

      (println)
      (println (ui/c :bold (format "Renamed: %d  Skipped: %d" @renamed @skipped)))
      (when-not apply
        (println (ui/c :dim "  Dry run. Pass --apply to perform the rename."))))))
