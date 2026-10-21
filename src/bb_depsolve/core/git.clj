(ns bb-depsolve.core.git
  "Git process helpers: status, commits-ahead, workspace auto-commit."
  (:require [babashka.fs :as fs]
            [babashka.process :as proc]
            [bb-depsolve.cli.ui :as ui]
            [clojure.string :as str]
            [bb-depsolve.core.verify :as verify]
            [hive-dsl.result :as r]))

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

(defn git-first-remote-url
  "URL of the project's first remote, or nil when it has none. `origin` wins
   when present (it is the remote push routing means); otherwise the first
   remote git lists. Reads git config only, never the network."
  [project-dir]
  (let [result (git project-dir "remote")]
    (when (zero? (:exit result))
      (let [names  (remove str/blank? (map str/trim (str/split-lines (:out result))))
            chosen (if (some #{"origin"} names) "origin" (first names))]
        (when chosen
          (let [r (git project-dir "remote" "get-url" chosen)]
            (when (zero? (:exit r))
              (let [url (str/trim (:out r))]
                (when-not (str/blank? url) url)))))))))

(defn remote-host
  "Lowercase hostname of a remote URL, or nil when it has none or is malformed.
   SCP-style SSH, URI-form SSH, HTTPS, and git protocol URLs are accepted.
   Pure: never consults git."
  [url]
  (let [url  (str/trim (or url ""))
        host (try
               (if (str/includes? url "://")
                 (.getHost (java.net.URI. url))
                 (when (and (str/includes? url "@") (str/includes? url ":"))
                   (let [authority (first (str/split url #":" 2))]
                     (second (str/split authority #"@" 2)))))
               (catch Exception _ nil))]
    (some-> host str/lower-case)))

(defn remote-provider
  "Classify a remote URL without consulting git. OVERRIDES maps lowercase hostnames
   to provider keywords; explicit choices take precedence over built-in hosts.
   SCP-style SSH, URI-form SSH, HTTPS, and git protocol URLs are accepted.
   Unknown or malformed hosts return :other, never an inferred GitHub provider."
  ([url] (remote-provider url {}))
  ([url overrides]
   (let [host (remote-host url)]
     (or (get overrides host)
         (cond
           (= host "github.com") :github
           (or (= host "gitlab.com") (str/starts-with? (or host "") "gitlab.")) :gitlab
           (= host "bitbucket.org") :bitbucket
           (or (= host "gitea.com") (str/starts-with? (or host "") "gitea.")) :gitea
           :else :other)))))

(defn git-upstream
  "Upstream ref of the current branch (e.g. \"origin/main\"), or nil."
  [project-dir]
  (let [result (git project-dir "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}")]
    (when (zero? (:exit result))
      (let [ref (str/trim (:out result))]
        (when-not (str/blank? ref) ref)))))

(defn git-tracking
  "Tracking metadata of the current branch, read from git config so branch
   names containing slashes are exact: {:branch :merge-ref :remote}.
   :branch is nil on a detached HEAD; :merge-ref and :remote are nil when the
   branch tracks nothing."
  [project-dir]
  (let [out (fn [& args]
              (let [r (apply git project-dir args)]
                (when (zero? (:exit r))
                  (let [s (str/trim (:out r))] (when-not (str/blank? s) s)))))
        branch (out "symbolic-ref" "--quiet" "--short" "HEAD")]
    {:branch branch
     :merge-ref (when branch (out "config" "--get" (str "branch." branch ".merge")))
     :remote (when branch (out "config" "--get" (str "branch." branch ".remote")))}))

(defn git-ahead-behind
  "Commits ahead of / behind UPSTREAM. Returns {:ahead n :behind n}."
  [project-dir upstream]
  (let [count-of (fn [range-spec]
                   (let [r (git project-dir "rev-list" "--count" range-spec)]
                     (if (zero? (:exit r))
                       (parse-long (str/trim (:out r)))
                       0)))]
    {:ahead (or (count-of (str upstream "..HEAD")) 0)
     :behind (or (count-of (str "HEAD.." upstream)) 0)}))

(defn git-dirty-files
  "Tracked files with uncommitted modifications. Untracked files are excluded:
   they cannot conflict with an incoming commit."
  [project-dir]
  (let [result (git project-dir "status" "--porcelain")]
    (if (zero? (:exit result))
      (->> (str/split-lines (:out result))
           (keep (fn [line]
                   (when (re-find #"^( M|M |MM|AM|A )" line)
                     (str/trim (subs line 3)))))
           vec)
      [])))

(defn git-incoming-files
  "Files the commits in HEAD..UPSTREAM touch."
  [project-dir upstream]
  (let [result (git project-dir "diff" "--name-only" (str "HEAD.." upstream))]
    (if (zero? (:exit result))
      (->> (str/split-lines (:out result)) (remove str/blank?) vec)
      [])))

(defn git-conflicted-files
  "Paths left unmerged after a failed merge, including modify/delete pairs."
  [project-dir]
  (let [unmerged (git project-dir "diff" "--name-only" "--diff-filter=U")
        status (git project-dir "status" "--porcelain")]
    (->> (concat (when (zero? (:exit unmerged)) (str/split-lines (:out unmerged)))
                 (when (zero? (:exit status))
                   (keep (fn [line]
                           (when (re-find #"^(DU|UD|AA|DD|UA|AU)" line)
                             (str/trim (subs line 3))))
                         (str/split-lines (:out status)))))
         (remove str/blank?)
         distinct
         sort
         vec)))

(defn git-push!
  "Push the current branch, then the tags, and answer BOTH results:
   {:branch result :tags result}. Two results because they answer two
   questions; folding them into one boolean reported forty-five landed pushes
   as failed when only their tags were refused (2026-09-17)."
  [project-dir]
  {:branch (git project-dir "push")
   :tags   (git project-dir "push" "--tags")})

(defn- git-out
  "Trimmed stdout of a git command in project-dir, or nil on failure or silence."
  [project-dir & args]
  (let [r (apply git project-dir args)]
    (when (zero? (:exit r))
      (let [s (str/trim (:out r))] (when-not (str/blank? s) s)))))

(defrecord GitPushProbe []
  verify/IPushProbe
  (expected-refs [_ project-dir]
    (let [{:keys [merge-ref remote]} (git-tracking project-dir)
          head (git-out project-dir "rev-parse" "HEAD")]
      (when (and head remote (str/starts-with? (or merge-ref "") "refs/heads/"))
        {:remote remote
         :refs (into [{:ref merge-ref :sha head :kind :branch}]
                     (for [tag (some-> (git-out project-dir "tag" "--points-at" "HEAD")
                                       str/split-lines)
                           :let [t (str/trim tag)
                                 sha (git-out project-dir "rev-parse" (str "refs/tags/" t))]
                           :when sha]
                       {:ref (str "refs/tags/" t) :sha sha :kind :tag}))})))
  (remote-refs [_ project-dir remote]
    (let [r (try
              (proc/sh {:dir (str project-dir)
                        :extra-env {"GIT_TERMINAL_PROMPT" "0"}}
                       "git" "ls-remote" "--heads" "--tags" remote)
              (catch Exception e {:exit -1 :err (ex-message e)}))]
      (if (zero? (:exit r))
        (r/ok (verify/parse-ls-remote (:out r)))
        (r/err :git/ls-remote-failed
               {:message (or (->> (str/split-lines (or (:err r) ""))
                                  (map str/trim)
                                  (remove str/blank?)
                                  first)
                             (str "git ls-remote exited " (:exit r)))})))))

(defn git-push-probe
  "The git adapter of IPushProbe: expectations from the local repo (HEAD, its
   tracked branch, the tags that point at HEAD) and observations from a read-only
   `git ls-remote`. Tags elsewhere in history are not checked: a push of old
   tags the remote refuses must not read as a failed release."
  []
  (->GitPushProbe))

(defn project-dir
  "The directory of PROJECT under ROOT-DIR. Discovery names a dep file that
   sits in the root itself after the root (its :project is the root's file
   name), which is every file at --depth 0, and <root>/<root-name> does not
   exist: that project's directory is the root."
  [root-dir project]
  (let [child (fs/path root-dir project)
        root  (fs/canonicalize root-dir)]
    (if (and (not (fs/directory? child))
             (= (str project) (str (fs/file-name root))))
      (str root)
      (str child))))

(defn- report-failure!
  "Print a red line for a git step that exited non-zero in PROJECT."
  [project step result]
  (let [why (->> (str/split-lines (str (:err result) "\n" (:out result)))
                 (map str/trim)
                 (remove str/blank?)
                 first)]
    (println (ui/c :red (str "  Commit failed: " project " (git " step " exited " (:exit result)
                             (when why (str ": " why)) ")")))))

(defn- auto-commit-project!
  "Commit changed dep files in a project with descriptive message.
   Returns true if a commit was made; a failed git step is printed."
  [project project-dir message]
  (let [changed (git-changed-files project-dir)]
    (when (seq changed)
      (doseq [f changed]
        (git project-dir "add" f))
      (let [result (git project-dir "commit" "-m" message)]
        (if (zero? (:exit result))
          true
          (do (report-failure! project "commit" result) false))))))

(defn auto-commit-workspace!
  "Commit all changed dep files across workspace projects.
   Public: used by bb-depsolve.wave."
  [root-dir dep-files message]
  (let [projects (->> dep-files
                      (map :project)
                      (distinct))]
    (doseq [project projects]
      (when (auto-commit-project! project (project-dir root-dir project) message)
        (println (ui/c :green (str "  Committed: " project)))))))

(defn commit-paths!
  "Commit exactly the files in PATHS-BY-PROJECT ({project [abs-path ...]}),
   one commit per project with MESSAGE, staging nothing else: a neighbour's
   work in progress in some other .edn of the same project stays where it
   is. Returns {project [relative-path ...]} for the projects that got a
   commit. A project with nothing staged is skipped quietly; a git step that
   fails is printed in red, never dropped."
  [root-dir paths-by-project message]
  (into {}
        (keep (fn [[project paths]]
                (let [dir (project-dir root-dir project)
                      rel (mapv #(str (fs/relativize dir %)) paths)]
                  (when (seq rel)
                    (let [a (apply git dir "add" "--" rel)]
                      (cond
                        (not (zero? (:exit a)))
                        (do (report-failure! project "add" a) nil)

                        (zero? (:exit (apply git dir "diff" "--cached" "--quiet" "--" rel)))
                        nil

                        :else
                        (let [c (apply git dir "commit" "-q" "-m" message "--" rel)]
                          (if (zero? (:exit c))
                            (do (println (ui/c :green (str "  Committed: " project " (" (str/join ", " rel) ")")))
                                [project rel])
                            (do (report-failure! project "commit" c) nil)))))))))
        paths-by-project))
