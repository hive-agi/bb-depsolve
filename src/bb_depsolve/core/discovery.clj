(ns bb-depsolve.core.discovery
  "Workspace scanning: dep-file discovery and project layout."
  (:require [babashka.fs :as fs]
            [bb-depsolve.version.api :as v]
            [clojure.string :as str]
            [bb-depsolve.core.skip :as skip]))

(def default-skip-dirs
  #{"vendor" "node_modules" ".git" "target" ".cpcache" ".lsp"})

(def default-depth 1)

(defn skip-path? [root-dir skip-dirs path]
  (let [rel (str (fs/relativize root-dir path))]
    (some #(or (= rel %)
               (str/starts-with? rel (str % "/")))
          skip-dirs)))

(defn main-checkout
  "The canonical checkout DIR is a linked git worktree of, or nil when DIR is
   not one (its .git is a directory, or absent, or unreadable).

   A linked worktree's .git is a FILE `gitdir: <repo>/.git/worktrees/<name>`,
   and that admin dir's `commondir` names the shared .git. Plain file reads:
   no git process is spawned."
  [dir]
  (let [dot-git (fs/path dir ".git")]
    (when (fs/regular-file? dot-git)
      (try
        (when-let [[_ gitdir] (re-find #"(?m)^gitdir:\s*(.+?)\s*$" (slurp (str dot-git)))]
          (let [gitdir (fs/path (fs/absolutize (fs/path dir)) gitdir)
                commondir-file (fs/path gitdir "commondir")
                common (when (fs/exists? commondir-file)
                         (fs/canonicalize (fs/path gitdir (str/trim (slurp (str commondir-file))))))]
            (when (and common (= ".git" (str (fs/file-name common))))
              (str (fs/parent common)))))
        (catch Exception _ nil)))))

(defn sibling-worktree?
  "True when DIR is a linked git worktree whose canonical checkout is also a
   direct child of ROOT-DIR. Such a directory is a branch of a project the
   scan already sees, not a project of its own: scanning it doubles the
   library and lets its dir name stand in for the lib. A worktree whose
   checkout lives elsewhere (a sweep directory of worktrees) is kept."
  [root-dir dir]
  (boolean
   (when-let [main (main-checkout dir)]
     (= (str (fs/canonicalize root-dir))
        (str (fs/parent (fs/canonicalize main)))))))

(defn excluded-dir?
  "True when a scan of ROOT-DIR leaves child DIR alone: it is named in
   SKIP-DIRS (which the caller has already unioned with the skip file), or it
   is a linked worktree of a sibling checkout."
  [root-dir skip-dirs dir]
  (boolean (or (skip-path? root-dir skip-dirs dir)
               (sibling-worktree? root-dir dir))))

(defn find-workspace-projects
  "Find all git-initialized subdirectories with VERSION files.
   Public: used by bb-depsolve.wave.

   The workspace's own skip file is unioned onto SKIP-DIRS, so a directory
   recorded there is left alone by every command without each one having to
   be passed --skip-dirs."
  [root-dir skip-dirs]
  (let [skip-dirs (into (set skip-dirs) (skip/dirs root-dir))]
    (->> (fs/list-dir root-dir)
         (filter fs/directory?)
         (remove #(excluded-dir? root-dir skip-dirs %))
         (filter #(fs/exists? (fs/path % "VERSION")))
         (filter #(fs/exists? (fs/path % ".git")))
         (sort)
         (vec))))

(def ignored-dep-files
  "Dep-file names that look like deps.edn but are not a committed dep file.

   local.deps.edn is the sanctioned home for :local/root overrides, and
   deps.lock.edn is generated output; scanning either would report the very
   thing the convention asks for."
  #{"local.deps.edn" "deps.lock.edn"})

(defn variant-dep-file?
  "True for a `deps.<name>.edn` sidecar, e.g. the deps.migrate.edn a Dockerfile
   copies in as deps.edn. These ARE committed dep files and are linted.

   Plain deps.edn is not one: it comes from the fixed name list, and reporting it
   here too would double every hit."
  [fname]
  (and (re-matches #"deps\..+\.edn" fname)
       (not (contains? ignored-dep-files fname))))

(defn find-dep-files
  "Find all deps.edn, deps.<name>.edn, bb.edn, and shadow-cljs.edn files in the
   workspace. The root's own dep files are always included, so --root can point
   directly AT a project, not only at the workspace container above it.

   The workspace's own skip file is unioned onto :skip-dirs. The root itself is
   never skipped: --root pointing AT a directory is an explicit instruction to
   scan it, which outranks a list that exists to bound a sweep."
  [{:keys [root skip-dirs depth]
    :or {root "." skip-dirs default-skip-dirs depth default-depth}}]
  (let [root-dir (str (fs/canonicalize root))
        skip-dirs (into (set skip-dirs) (skip/dirs root-dir))
        scan-dirs (if (pos? depth)
                    (into [root-dir]
                          (->> (fs/list-dir root-dir)
                               (filter fs/directory?)
                               (remove #(excluded-dir? root-dir skip-dirs %))
                               (sort)))
                    [root-dir])
        names (fn [dir]
                (->> (concat ["deps.edn" "bb.edn" "shadow-cljs.edn"]
                             (when (fs/directory? dir)
                               (->> (fs/list-dir dir)
                                    (map (comp str fs/file-name))
                                    (filter variant-dep-file?)
                                    (sort))))
                     (distinct)))]
    (->> (for [dir scan-dirs
               fname (names dir)
               :let [f (fs/path dir fname)]
               :when (fs/exists? f)]
           {:path    (str f)
            :type    (keyword (str/replace fname "." "-"))
            :project (str (fs/file-name dir))})
         (vec))))

(defn shadow-deps-file?
  "True if the dep file is a shadow-cljs.edn. Public: used by audit ns."
  [{:keys [type]}]
  (= type :shadow-cljs-edn))

(defn extract-mvn-deps
  "Extract mvn deps from a dep file, dispatching by file type.
   deps.edn/bb.edn: uses :mvn/version format.
   shadow-cljs.edn: uses Lein-style [lib \"ver\"] from :dependencies.
   Returns vec of {:lib :version :match}."
  [{:keys [type]} content]
  (if (= type :shadow-cljs-edn)
    (v/find-shadow-deps content)
    (v/find-mvn-deps content)))
