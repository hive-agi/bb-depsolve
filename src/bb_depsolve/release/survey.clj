(ns bb-depsolve.release.survey
  "Read-only git survey of the workspace: per repo, the branch, how far it is
   from its upstream, how dirty it is, its last tag, its VERSION, its remotes
   and the default branch the remote advertises.

   The row is data for the release plan formatter and the cascade executor.
   Nothing here mutates a repo: every git call is a read, and the runner is a
   parameter (GIT-FN, [dir & args] -> {:exit :out}) so a caller can swap it.

   row {:project :dir :branch :upstream :ahead :behind :dirty :untracked
        :last-tag :version :remotes :default-branch}

   Honest nils: :branch is nil on a detached HEAD; :upstream, :ahead and
   :behind are nil when the branch tracks nothing; :last-tag is nil without
   tags; :default-branch is nil when no remote HEAD is known locally (the
   survey never calls a forge API, it reads refs/remotes/<remote>/HEAD).
   A repo that cannot be read comes back as {:project :dir :error msg}."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.git :as git]
            [clojure.string :as str]))

(defn- signed-count
  "Count from a signed token such as \"+3\" or \"-1\", or nil."
  [token]
  (when (and token (> (count token) 1))
    (parse-long (subs token 1))))

(defn- header-value
  "Value of the `# PREFIX value` header line in LINES, or nil."
  [lines prefix]
  (some #(when (str/starts-with? % prefix) (subs % (count prefix))) lines))

(defn parse-status-v2
  "Parse `git status --porcelain=v2 --branch` output.
   => {:branch :upstream :ahead :behind :dirty :untracked}
   :dirty counts tracked entries with changes (including unmerged); :untracked
   counts untracked paths; ignored paths count as neither."
  [output]
  (let [lines (into [] (remove str/blank?) (str/split-lines (or output "")))
        head (header-value lines "# branch.head ")
        [ahead behind] (str/split (or (header-value lines "# branch.ab ") "") #" ")
        entries (remove #(str/starts-with? % "#") lines)]
    {:branch (when-not (contains? #{nil "(detached)"} head) head)
     :upstream (header-value lines "# branch.upstream ")
     :ahead (signed-count ahead)
     :behind (signed-count behind)
     :dirty (count (remove #(contains? #{\? \!} (first %)) entries))
     :untracked (count (filter #(= \? (first %)) entries))}))

(defn parse-remote-urls
  "Parse `git config --get-regexp ^remote\\..*\\.url$` output, lines of
   `remote.NAME.url VALUE`. => sorted map NAME -> first URL configured."
  [output]
  (reduce (fn [acc line]
            (let [line (str/trim line)
                  sep (str/index-of line " ")
                  k (when sep (subs line 0 sep))
                  url (when sep (str/trim (subs line sep)))]
              (if (and k
                       (str/starts-with? k "remote.")
                       (str/ends-with? k ".url")
                       (> (count k) (count "remote..url"))
                       (not (str/blank? url)))
                (let [remote (subs k (count "remote.") (- (count k) (count ".url")))]
                  (if (contains? acc remote) acc (assoc acc remote url)))
                acc)))
          (sorted-map)
          (str/split-lines (or output ""))))

(defn parse-remote-head
  "Default branch from `git symbolic-ref --short refs/remotes/REMOTE/HEAD`
   output (e.g. \"origin/main\"), or nil."
  [remote output]
  (let [ref (str/trim (or output ""))
        prefix (str remote "/")]
    (when (and (str/starts-with? ref prefix) (> (count ref) (count prefix)))
      (subs ref (count prefix)))))

(defn default-remote
  "The remote the default branch is read from: origin when configured, else
   the first remote by name, else nil."
  [remotes]
  (cond (contains? remotes "origin") "origin"
        (seq remotes) (first (keys remotes))))

(defn read-repo
  "Survey the repo at DIR through GIT-FN. Pure read; never throws."
  [git-fn dir]
  (let [dir (str dir)
        project (str (fs/file-name dir))
        run (fn [& args]
              (let [result (apply git-fn dir args)]
                (when (zero? (long (:exit result)))
                  (:out result))))]
    (try
      (if-let [status (run "status" "--porcelain=v2" "--branch")]
        (let [remotes (parse-remote-urls (run "config" "--get-regexp" "^remote\\..*\\.url$"))
              remote (default-remote remotes)
              version-file (fs/path dir "VERSION")]
          (merge {:project project :dir dir}
                 (parse-status-v2 status)
                 {:last-tag (some-> (run "describe" "--tags" "--abbrev=0") str/trim not-empty)
                  :version (when (fs/exists? version-file)
                             (not-empty (str/trim (slurp (str version-file)))))
                  :remotes remotes
                  :default-branch (when remote
                                    (parse-remote-head
                                     remote
                                     (run "symbolic-ref" "--short"
                                          (str "refs/remotes/" remote "/HEAD"))))}))
        {:project project :dir dir :error "git status failed: not a readable git repo"})
      (catch Exception e
        {:project project :dir dir :error (or (ex-message e) (str e))}))))

(defn survey-workspace
  "Survey every git project with a VERSION file directly under ROOT, sorted by
   project. OPTS: :root (default \".\"), :skip-dirs (default the discovery
   defaults), :git-fn (default bb-depsolve.core.git/git). Repos are read in
   parallel: each is a handful of short read-only git processes."
  [{:keys [root skip-dirs git-fn]
    :or {root "." skip-dirs discovery/default-skip-dirs git-fn git/git}}]
  (let [root-dir (str (fs/canonicalize root))]
    (->> (discovery/find-workspace-projects root-dir skip-dirs)
         (pmap #(read-repo git-fn %))
         (sort-by :project)
         vec)))
