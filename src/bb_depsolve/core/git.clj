(ns bb-depsolve.core.git
  "Git process helpers: status, commits-ahead, workspace auto-commit."
  (:require [babashka.fs :as fs]
            [babashka.process :as proc]
            [bb-depsolve.ui :as ui]
            [clojure.string :as str]))

(defn git
  "Run a git command in project-dir. Returns process result map.
   Public: used by bb-depsolve.wave."
  [project-dir & args]
  (proc/sh (into ["git" "-C" (str project-dir)] args)))

(defn- git-changed-files
  "Get list of changed (tracked + untracked dep) files in a project dir."
  [project-dir]
  (let [result (git project-dir "diff" "--name-only" "--" "*.edn" ".gitignore")]
    (when (zero? (:exit result))
      (->> (str/split-lines (:out result))
           (remove str/blank?)
           (vec)))))

(defn git-commits-ahead
  "Count commits ahead of a tag. Returns 0 if tag doesn't exist.
   Public: used by bb-depsolve.wave."
  [project-dir tag]
  (let [result (git project-dir "log" "--oneline" (str tag "..HEAD"))]
    (if (zero? (:exit result))
      (count (remove str/blank? (str/split-lines (:out result))))
      0)))

(defn git-has-remote?
  "Check if project has at least one remote configured.
   Public: used by bb-depsolve.wave."
  [project-dir]
  (let [result (git project-dir "remote")]
    (and (zero? (:exit result))
         (not (str/blank? (:out result))))))

(defn- auto-commit-project!
  "Commit changed dep files in a project with descriptive message.
   Returns true if a commit was made."
  [project-dir message]
  (let [changed (git-changed-files project-dir)]
    (when (seq changed)
      (doseq [f changed]
        (git project-dir "add" f))
      (let [result (git project-dir "commit" "-m" message)]
        (zero? (:exit result))))))

(defn auto-commit-workspace!
  "Commit all changed dep files across workspace projects.
   Public: used by bb-depsolve.wave."
  [root-dir dep-files message]
  (let [projects (->> dep-files
                      (map :project)
                      (distinct))]
    (doseq [project projects
            :let [project-dir (str (fs/path root-dir project))]]
      (when (auto-commit-project! project-dir message)
        (println (ui/c :green (str "  Committed: " project)))))))
