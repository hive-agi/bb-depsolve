(ns bb-depsolve.core.bump
  "The bump command: VERSION bump, tag, push."
  (:require [babashka.fs :as fs]
            [babashka.process :as proc]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.sync :as sync]
            [bb-depsolve.ui :as ui]
            [bb-depsolve.version :as v]
            [clojure.string :as str]))

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

(defn bump-cmd
  "Bump VERSION file, git commit + tag + push, optionally sync downstream.
   When --stable bumps to v1.0.0+ or any major increment beyond v0,
   warns about workspace consumers (compat audit). Pass --force to skip the
   confirmation prompt."
  [{:keys [opts]}]
  (let [{:keys [root major minor stable sync org force]
         :or {root "."}} opts
        project-dir (str (fs/canonicalize root))
        version-file (str (fs/path project-dir "VERSION"))]

    (when-not (fs/exists? version-file)
      (println (ui/c :red (str "Error: VERSION file not found at " version-file)))
      (System/exit 1))

    (let [current-str  (str/trim (slurp version-file))
          current      (v/parse-semver current-str)]

      (when-not current
        (println (ui/c :red (str "Error: Cannot parse version '" current-str "'")))
        (System/exit 1))

      (let [bump-fn      (cond stable v/bump-major
                               major  v/bump-minor
                               minor  v/bump-patch
                               :else  v/bump-patch)
            new-semver   (bump-fn current)
            new-version  (v/semver->version new-semver)
            new-tag      (v/semver->tag new-semver)
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
          (println (ui/c :dim "  Run `bb-depsolve sync --apply` after bump to align."))
          (when-not force
            (println (ui/c :dim "  Pass --force to bypass this warning."))
            (System/exit 1)))

        (println (ui/c :bold (str "Bumping " current-str " -> " new-version)))
        (println)

        (spit version-file (str new-version "\n"))
        (println (ui/c :green (str "  Updated VERSION: " new-version)))

        (let [extra-version-files (->> (fs/glob project-dir "**/VERSION")
                                       (map str)
                                       (remove #{version-file}))]
          (doseq [f extra-version-files]
            (spit f (str new-version "\n"))
            (println (ui/c :green (str "  Updated " (str (fs/relativize project-dir f)))))))

        (let [run-git (fn [& args]
                        (let [result (proc/sh (into ["git" "-C" project-dir] args))]
                          (when-not (zero? (:exit result))
                            (println (ui/c :yellow (str "  git " (first args) ": "
                                                        (str/trim (:err result ""))))))
                          result))
              all-version-files (into ["VERSION"]
                                      (->> (fs/glob project-dir "**/VERSION")
                                           (map #(str (fs/relativize project-dir %)))))]
          (doseq [f all-version-files]
            (run-git "add" f))
          (run-git "commit" "-m" (str "release: " new-tag))
          (println (ui/c :green (str "  Committed: release: " new-tag)))

          (run-git "tag" new-tag)
          (println (ui/c :green (str "  Tagged: " new-tag)))

          (run-git "push")
          (run-git "push" "--tags")
          (println (ui/c :green "  Pushed to remote")))

        (println)

        (when (and sync org)
          (println (ui/c :bold "Running sync..."))
          (sync/sync-cmd {:opts {:root (str (fs/parent project-dir))
                                 :org org :apply true}}))

        (println (ui/c :green (str "Done: " new-tag)))))))
