(ns bb-depsolve.core
  "Monorepo dependency sync, upgrade & reporting.

   Layer 2 (Action/Orchestration): Performs I/O, delegates to version.clj
   for pure calculations. Uses hive-dsl Result at I/O boundaries.

   Architecture:
     version.clj (Calculation) — pure parsing, comparison, string transforms
     core.clj    (Action)      — I/O, resolution, command orchestration
     cli.clj     (Interaction) — CLI dispatch, arg coercion"
  (:require [babashka.fs :as fs]
            [babashka.process :as proc]
            [babashka.http-client :as http]
            [bblgum.core :as gum]
            [cheshire.core :as json]
            [clojure.string :as str]
            [hive-dsl.bounded-atom :as ba]
            [hive-dsl.gate :as gate]
            [hive-dsl.result :as r]
            [bb-depsolve.version :as v]))

(def ^:private http-gate (gate/gate {:permits 5 :timeout-ms 30000}))

(def ^:private colors
  {:red     "\033[31m"
   :green   "\033[32m"
   :yellow  "\033[33m"
   :cyan    "\033[36m"
   :bold    "\033[1m"
   :dim     "\033[2m"
   :reset   "\033[0m"})

(defn c
  "Public: wrap STRING in ANSI color code. Used by sibling nses (audit, etc)."
  [color & parts]
  (str (get colors color "") (apply str parts) (:reset colors)))

(defn- visible-len [s]
  (count (str/replace s #"\033\[[0-9;]*m" "")))

(defn- pad-right [s width]
  (let [vlen (visible-len s)
        padding (max 0 (- width vlen))]
    (str s (apply str (repeat padding \space)))))

;; =============================================================================
;; Git helpers (auto-commit, push, workspace ops)
;; =============================================================================

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
        (println (c :green (str "  Committed: " project)))))))

(def default-skip-dirs
  #{"vendor" "node_modules" ".git" "target" ".cpcache" ".lsp"})

(def default-depth 1)

(defn skip-path? [root-dir skip-dirs path]
  (let [rel (str (fs/relativize root-dir path))]
    (some #(or (= rel %)
               (str/starts-with? rel (str % "/")))
          skip-dirs)))

(defn find-workspace-projects
  "Find all git-initialized subdirectories with VERSION files.
   Public: used by bb-depsolve.wave."
  [root-dir skip-dirs]
  (->> (fs/list-dir root-dir)
       (filter fs/directory?)
       (remove #(skip-path? root-dir skip-dirs %))
       (filter #(fs/exists? (fs/path % "VERSION")))
       (filter #(fs/exists? (fs/path % ".git")))
       (sort)
       (vec)))

(defn find-dep-files
  "Find all deps.edn, bb.edn, and shadow-cljs.edn files in the workspace."
  [{:keys [root skip-dirs depth]
    :or {root "." skip-dirs default-skip-dirs depth default-depth}}]
  (let [root-dir (str (fs/canonicalize root))
        scan-dirs (if (pos? depth)
                    (->> (fs/list-dir root-dir)
                         (filter fs/directory?)
                         (remove #(skip-path? root-dir skip-dirs %))
                         (sort))
                    [root-dir])]
    (->> (for [dir scan-dirs
               fname ["deps.edn" "bb.edn" "shadow-cljs.edn"]
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

(defn apply-mvn-change!
  "Apply a single mvn version change to file content, dispatching by file type.
   Returns updated content string."
  [content dep-file lib new-version]
  (if (shadow-deps-file? dep-file)
    (v/update-shadow-dep content lib new-version)
    (v/update-mvn-dep content lib new-version)))

(def ^:private github-url "https://github.com/%s/%s")

(defn resolve-local-tags
  "Resolve all tags from a local git repo. Returns Result<[{:tag :sha}]>."
  [repo-dir]
  (r/try-effect*
   :io/git-local-tags
   (let [result (proc/sh ["git" "-C" (str repo-dir) "tag" "--sort=-version:refname"
                          "-l" "v*" "--format=%(refname:short) %(objectname:short)"])]
     (if (zero? (:exit result))
       (->> (str/split-lines (:out result))
            (remove str/blank?)
            (mapv (fn [line]
                    (let [[tag sha] (str/split line #"\s+" 2)]
                      {:tag tag :sha sha}))))
       (throw (ex-info "git tag failed" {:exit (:exit result)}))))))

(defn resolve-remote-tags
  "Resolve tags from GitHub via git ls-remote. Returns Result<[{:tag :sha :sha-short}]>."
  [org repo]
  (r/try-effect*
   :io/git-remote-tags
   (let [url (format github-url org repo)
         result (proc/sh ["git" "ls-remote" "--tags" "--sort=-version:refname" url])]
     (if (zero? (:exit result))
       (->> (str/split-lines (:out result))
            (remove str/blank?)
            (remove #(str/includes? % "^{}"))
            (mapv (fn [line]
                    (let [[sha ref] (str/split line #"\t" 2)
                          tag (str/replace ref "refs/tags/" "")]
                      {:tag tag :sha sha :sha-short (subs sha 0 7)}))))
       (throw (ex-info "git ls-remote failed" {:exit (:exit result)}))))))

(defn resolve-lib-tags
  "Resolve the latest tag+sha for a git lib.
   Uses GitHub remote first, falls back to local clone.
   Returns Result<{:tag :sha :source}>."
  [root-dir lib-sym dir-name]
  (if-let [{:keys [org repo]} (v/parse-github-lib lib-sym)]
    (let [remote-result (resolve-remote-tags org repo)]
      (if (and (r/ok? remote-result) (seq (:ok remote-result)))
        (if-let [latest (v/latest-tag (:ok remote-result))]
          (r/ok (assoc latest :source :remote))
          (r/err :parse/no-semver-tags {:lib lib-sym}))
        (let [local-dir (fs/path root-dir dir-name)]
          (if (fs/directory? (fs/path local-dir ".git"))
            (r/let-ok [tags (resolve-local-tags local-dir)]
                      (if-let [latest (v/latest-tag tags)]
                        (r/ok (assoc latest :source :local))
                        (r/err :parse/no-semver-tags {:lib lib-sym})))
            (r/err :io/not-found {:lib lib-sym :dir (str local-dir)})))))
    (r/err :parse/not-github-lib {:lib lib-sym})))

(defn resolve-clojars-latest
  "Query Clojars API for latest release version. Returns Result<string>."
  [group-id artifact-id]
  (r/try-effect*
   :io/clojars
   (let [url (format "https://clojars.org/api/artifacts/%s/%s" group-id artifact-id)
         resp (http/get url {:headers {"Accept" "application/json"} :throw false})]
     (if (= 200 (:status resp))
       (or (-> (json/parse-string (:body resp) true) :latest_release)
           (throw (ex-info "No latest_release" {:group group-id :artifact artifact-id})))
       (throw (ex-info "Clojars HTTP error" {:status (:status resp)}))))))

(defn resolve-maven-latest
  "Query Maven Central for latest version. Returns Result<string>."
  [group-id artifact-id]
  (r/try-effect*
   :io/maven-central
   (let [url (format "https://search.maven.org/solrsearch/select?q=g:%%22%s%%22+AND+a:%%22%s%%22&rows=1&wt=json"
                     group-id artifact-id)
         resp (http/get url {:throw false})]
     (if (= 200 (:status resp))
       (or (-> (json/parse-string (:body resp) true) :response :docs first :latestVersion)
           (throw (ex-info "No latestVersion" {:group group-id :artifact artifact-id})))
       (throw (ex-info "Maven HTTP error" {:status (:status resp)}))))))

(defn resolve-mvn-latest
  "Resolve latest stable version. Tries Clojars, falls back to Maven Central.
   Filters pre-releases unless allow-pre? is true."
  [lib-sym allow-pre?]
  (let [[group artifact] (str/split (str lib-sym) #"/")
        group (or group artifact)
        artifact (or artifact group)
        clojars (resolve-clojars-latest group artifact)
        latest-r (if (r/ok? clojars)
                   clojars
                   (resolve-maven-latest group artifact))]
    (r/bind latest-r
            (fn [latest]
              (if (and (not allow-pre?) (v/pre-release? latest))
                (r/err :parse/pre-release {:version latest :lib lib-sym})
                (r/ok latest))))))

(defn discover-internal-libs
  "Auto-discover internal git deps by scanning dep files for io.github.{org}/* coords.
   Returns map of lib-sym -> dir-name."
  [dep-files org]
  (->> dep-files
       (remove shadow-deps-file?)
       (mapcat (fn [{:keys [path]}]
                 (v/find-git-deps (slurp path))))
       (filter #(v/lib-matches-org? org (:lib %)))
       (map (fn [{:keys [lib]}]
              [lib (v/lib-artifact-id lib)]))
       (into {})))

(defn compute-sync-changes
  "Compute sync changes between dep files and resolved lib versions. Pure."
  [dep-files resolved]
  (->> dep-files
       (remove shadow-deps-file?)
       (mapcat (fn [{:keys [path project]}]
                 (let [content (slurp path)
                       git-deps (v/find-git-deps content)]
                   (for [{:keys [lib tag sha]} git-deps
                         :when (contains? resolved lib)
                         :let [resolved-info (get resolved lib)
                               rtag (:tag resolved-info)
                               rsha (v/pick-sha sha resolved-info)]
                         :when (or (not= tag rtag) (not (v/sha-matches? sha rsha)))]
                     {:path path :project project :lib lib
                      :old-tag tag :old-sha sha
                      :new-tag rtag :new-sha rsha}))))
       (vec)))

(defn apply-git-changes!
  "Apply git dep changes to files. Action: writes to disk."
  [root-dir changes]
  (let [by-file (group-by :path changes)]
    (doseq [[path file-changes] by-file
            :let [content (atom (slurp path))]]
      (doseq [{:keys [lib new-tag new-sha]} file-changes]
        (swap! content v/update-git-dep lib new-tag new-sha))
      (spit path @content)
      (println (c :green (str "  Updated " (str (fs/relativize root-dir path))))))
    (println)
    (println (c :green (format "Applied %d changes." (count changes))))))

(defn apply-mvn-upgrades!
  "Apply mvn version upgrades to files. Action: writes to disk.
   Dispatches to the correct update fn based on file type."
  [root-dir upgrades dep-file-index]
  (let [by-file (group-by :path upgrades)]
    (doseq [[path file-upgrades] by-file
            :let [content (atom (slurp path))
                  dep-file (get dep-file-index path)]]
      (doseq [{:keys [lib new-version]} file-upgrades]
        (swap! content apply-mvn-change! dep-file lib new-version))
      (spit path @content)
      (println (c :green (str "  Updated " (str (fs/relativize root-dir path))))))
    (println)
    (println (c :green (format "Applied %d upgrades across %d files."
                               (count upgrades) (count by-file))))))

(defn- tty? []
  (zero? (:exit (proc/sh ["test" "-t" "0"] {:continue true}))))

(defn- matrix->csv [multi-project all-projects]
  (let [header (str/join "," (cons "Library" all-projects))
        rows (for [[lib project-versions] multi-project]
               (str/join ","
                         (cons (str lib)
                               (for [p all-projects]
                                 (or (get project-versions p) "-")))))]
    (str/join "\n" (cons header rows))))

(defn- gum-table [csv multi-project all-projects]
  (if (tty?)
    (gum/gum :table :in (.getBytes csv))
    (let [lib-col 45
          ver-col 13
          abbrev (fn [s w] (subs s 0 (min w (count s))))]
      (print (pad-right "Library" lib-col))
      (doseq [p all-projects]
        (print "  " (pad-right (abbrev p ver-col) ver-col)))
      (println)
      (println (apply str (repeat (+ lib-col (* (+ 2 ver-col) (count all-projects))) \-)))
      (doseq [[lib project-versions] multi-project
              :let [versions (set (vals project-versions))
                    drift? (> (count versions) 1)]]
        (print (pad-right (str lib) lib-col))
        (doseq [p all-projects
                :let [v* (get project-versions p)
                      display (if v* (abbrev v* ver-col) "-")
                      colored (cond
                                (nil? v*) (c :dim "-")
                                drift?    (c :yellow display)
                                :else     (c :dim display))]]
          (print "  " (pad-right colored ver-col)))
        (println)))))

(defn- gum-filter [choices header]
  (when (tty?)
    (let [{:keys [status result]} (gum/gum :filter choices
                                           :no-limit true
                                           :header header)]
      (when (= 0 status) result))))

(defn sync-cmd
  "Sync internal git deps across all workspace projects."
  [{:keys [opts]}]
  (let [{:keys [root org apply commit skip-dirs depth]
         :or {root "." depth default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   default-skip-dirs)
        dep-files (find-dep-files {:root root :skip-dirs skip-set :depth depth})]

    (when-not org
      (println (c :red "Error: --org is required for sync (e.g. --org hive-agi)"))
      (System/exit 1))

    (let [internal-libs (discover-internal-libs dep-files org)]
      (println (c :bold (format "Resolving %s tags (%d libs)..." (str "io.github." org) (count internal-libs))))
      (println)

      (let [resolved (into {}
                           (for [[lib-sym dir-name] internal-libs
                                 :let [result (resolve-lib-tags root-dir lib-sym dir-name)]
                                 :when (r/ok? result)]
                             [lib-sym (:ok result)]))]

        (doseq [[lib-sym {:keys [tag sha-short sha source]}] (sort-by (comp str key) resolved)]
          (printf "  %-40s %s -> %s  (%s)\n"
                  (c :cyan (str lib-sym))
                  (c :green tag)
                  (c :dim (or sha-short sha))
                  (name source)))
        (println)

        (println (c :bold (format "Scanning %d dep files..." (count dep-files))))
        (println)

        (let [changes (compute-sync-changes dep-files resolved)]
          (if (empty? changes)
            (println (c :green "All internal deps are in sync."))
            (do
              (println (c :yellow (format "%d mismatches found:" (count changes))))
              (println)
              (doseq [{:keys [project lib old-tag old-sha new-tag new-sha]} changes]
                (printf "  %-25s %-35s %s %s -> %s %s\n"
                        (c :cyan project) (str lib)
                        (c :red old-tag) (c :dim old-sha)
                        (c :green new-tag) (c :dim new-sha)))
              (println)
              (if apply
                (do (apply-git-changes! root-dir changes)
                    (when commit
                      (auto-commit-workspace! root-dir dep-files
                                              "chore: sync internal git deps (bb-depsolve)")))
                (println (c :dim "  Dry run. Pass --apply to write changes."))))))))))

(defn upgrade-cmd
  "Check for newer versions of all dependencies."
  [{:keys [opts]}]
  (let [{:keys [root apply commit skip-dirs depth pre-release]
         :or {root "." depth default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   default-skip-dirs)
        dep-files (find-dep-files {:root root :skip-dirs skip-set :depth depth})
        dep-file-index (into {} (map (fn [df] [(:path df) df]) dep-files))]

    (println (c :bold "Checking latest versions..."))
    (println)

    (let [all-mvn-deps (atom {})
          file-deps (atom [])]

      (doseq [{:keys [path project] :as dep-file} dep-files
              :let [content (slurp path)
                    mvn-deps (extract-mvn-deps dep-file content)]]
        (doseq [{:keys [lib version]} mvn-deps]
          (swap! all-mvn-deps update lib (fnil conj #{}) version)
          (swap! file-deps conj {:path path :project project
                                 :lib lib :version version})))

      (let [unique-libs (keys @all-mvn-deps)
            _ (printf "  Checking %d unique libraries...\n" (count unique-libs))
            latest-versions (atom {})]

        (doseq [[i lib] (map-indexed vector (sort-by str unique-libs))]
          (when (zero? (mod i 10))
            (printf "\r  [%d/%d] %s" (inc i) (count unique-libs) (c :dim (str lib)))
            (flush))
          (let [result (resolve-mvn-latest lib (boolean pre-release))]
            (when (r/ok? result)
              (swap! latest-versions assoc lib (:ok result)))))

        (println "\r  " (c :green (format "Resolved %d / %d libraries" (count @latest-versions) (count unique-libs))))
        (println)

        (let [upgrades (->> @file-deps
                            (filter (fn [{:keys [lib version]}]
                                      (let [latest (get @latest-versions lib)]
                                        (and latest
                                             (not= version latest)
                                             (v/version-newer? version latest)))))
                            (mapv (fn [{:keys [path project lib version]}]
                                    {:path path :project project :lib lib
                                     :old-version version
                                     :new-version (get @latest-versions lib)}))
                            (distinct))]

          (if (empty? upgrades)
            (println (c :green "All mvn deps are up to date."))
            (let [by-lib (->> upgrades
                              (group-by :lib)
                              (map (fn [[lib entries]]
                                     (let [e (first entries)]
                                       {:lib lib
                                        :old-version (:old-version e)
                                        :new-version (:new-version e)
                                        :projects (mapv :project entries)})))
                              (sort-by (comp str :lib)))]

              (println (c :yellow (format "%d upgrades available across %d libraries:"
                                          (count upgrades) (count by-lib))))
              (println)

              (doseq [{:keys [lib old-version new-version projects]} by-lib]
                (printf "  %-40s %s -> %s  (%s)\n"
                        (str lib)
                        (c :red old-version)
                        (c :green new-version)
                        (c :dim (str/join ", " projects))))
              (println)

              (if apply
                (let [choices (mapv #(format "%-40s  %s -> %s  (%s)"
                                             (str (:lib %))
                                             (:old-version %)
                                             (:new-version %)
                                             (str/join ", " (:projects %)))
                                    by-lib)
                      selected (or (gum-filter choices
                                               "Select upgrades (tab=toggle, enter=confirm)")
                                   (do (println (c :dim "No TTY — applying all upgrades."))
                                       choices))]
                  (if (empty? selected)
                    (println (c :dim "No upgrades selected."))
                    (let [selected-libs (->> selected
                                             (map #(-> % str/trim (str/split #"\s+" 2) first symbol))
                                             (set))
                          selected-upgrades (filter #(contains? selected-libs (:lib %)) upgrades)]
                      (apply-mvn-upgrades! root-dir selected-upgrades dep-file-index)
                      (when commit
                        (auto-commit-workspace! root-dir dep-files
                                                "chore: upgrade deps to latest (bb-depsolve)")))))
                (println (c :dim "  Dry run. Pass --apply for interactive selection."))))))))))

(defn report-cmd
  "Show a dependency matrix across all projects."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs depth]
         :or {root "." depth default-depth}} opts
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   default-skip-dirs)
        dep-files (find-dep-files {:root root :skip-dirs skip-set :depth depth})
        matrix (atom (sorted-map))]

    (doseq [{:keys [path project] :as dep-file} dep-files
            :let [content (slurp path)
                  mvn-deps (extract-mvn-deps dep-file content)
                  git-deps (if (shadow-deps-file? dep-file)
                             []
                             (v/find-git-deps content))]]
      (doseq [{:keys [lib version]} mvn-deps]
        (swap! matrix assoc-in [lib project] version))
      (doseq [{:keys [lib tag sha]} git-deps]
        (swap! matrix assoc-in [lib project] (str tag " " sha))))

    (let [multi-project (->> @matrix
                             (filter (fn [[_ projs]] (> (count projs) 1)))
                             (into (sorted-map)))
          all-projects (->> (vals multi-project)
                            (mapcat keys)
                            (distinct)
                            (sort))
          csv (matrix->csv multi-project all-projects)
          drift-count (count (filter (fn [[_ pv]] (> (count (set (vals pv))) 1)) multi-project))]

      (println (c :bold "Dependency Matrix"))
      (println (c :bold (format "%d libraries shared, %d with version drift"
                                (count multi-project) drift-count)))
      (println)
      (gum-table csv multi-project all-projects))))

(defn- format-local-dep-warning
  "Format a warning line for a :local/root dep."
  [project lib path]
  (format "  %-25s %-35s %s"
          (c :cyan project) (str lib) (c :yellow path)))

(defn- ensure-gitignore-entry!
  "Add entry to .gitignore if not already present."
  [root-dir entry]
  (let [gitignore (str (fs/path root-dir ".gitignore"))
        content (if (fs/exists? gitignore) (slurp gitignore) "")
        lines (str/split-lines content)]
    (when-not (some #(= (str/trim %) entry) lines)
      (spit gitignore (str content (when-not (str/ends-with? content "\n") "\n") entry "\n"))
      (println (c :green (str "  Added '" entry "' to .gitignore"))))))

(defn- generate-local-deps-edn
  "Generate local.deps.edn content from local dep entries."
  [local-entries]
  (let [header ";; local.deps.edn — machine-specific overrides, DO NOT COMMIT\n;; Auto-generated by bb-depsolve lint --fix\n;;\n;; Usage with clj:  clj -Sdeps \"$(cat local.deps.edn)\"\n;; Usage with bb:    add {:local/root ...} overrides to bb.edn aliases\n"
        deps-str (->> local-entries
                      (map (fn [{:keys [lib path]}]
                             (str "  " lib " {:local/root \"" path "\"}")))
                      (str/join "\n"))]
    (str header "\n{:deps\n {" (str/trim deps-str) "}}\n")))

(defn lint-cmd
  "Lint dep files for anti-patterns. Currently checks for :local/root in deps.edn."
  [{:keys [opts]}]
  (let [{:keys [root org fix skip-dirs depth]
         :or {root "." depth default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   default-skip-dirs)
        dep-files (find-dep-files {:root root :skip-dirs skip-set :depth depth})]

    (println (c :bold "Linting dep files for anti-patterns..."))
    (println)

    (let [all-locals (atom [])
          by-file (atom {})]

      (doseq [{:keys [path project]} (remove shadow-deps-file? dep-files)
              :let [content (slurp path)
                    locals (v/find-local-deps content)]
              :when (seq locals)]
        (doseq [entry locals]
          (swap! all-locals conj (assoc entry :path path :project project)))
        (swap! by-file assoc path {:project project :locals locals :content content}))

      (if (empty? @all-locals)
        (do
          (println (c :green "No anti-patterns found. All clean!"))
          (println))
        (do
          (println (c :yellow (str "WARNING: " (count @all-locals) " :local/root dep(s) found in committed dep files")))
          (println)
          (println (c :dim "  :local/root pins deps to machine-specific paths."))
          (println (c :dim "  This breaks CI, other developers, and production builds."))
          (println (c :dim "  Move local overrides to local.deps.edn (gitignored) instead."))
          (println)

          (doseq [{:keys [project lib path]} @all-locals
                  :let [local-path (:path (first (filter #(= (:lib %) lib) (get-in @by-file [path :locals]))))]]
            (println (format-local-dep-warning project lib local-path)))
          (println)

          (if fix
            (do
              (println (c :bold "Fixing: splitting :local/root deps..."))
              (println)

              (let [unique-locals (->> @all-locals
                                       (map #(select-keys % [:lib :path]))
                                       (distinct))]

                (doseq [[file-path {:keys [project locals content]}] @by-file
                        :let [project-dir (str (fs/parent file-path))]]

                  (let [local-deps-path (str (fs/path project-dir "local.deps.edn"))]
                    (if (fs/exists? local-deps-path)
                      (println (c :yellow (str "  Skipped " (str (fs/relativize root-dir local-deps-path))
                                               " (already exists — merge manually)")))
                      (do
                        (spit local-deps-path (generate-local-deps-edn locals))
                        (println (c :green (str "  Created " (str (fs/relativize root-dir local-deps-path))))))))

                  (ensure-gitignore-entry! project-dir "local.deps.edn")

                  (let [updated-content (atom content)
                        replaced (atom 0)]
                    (doseq [{:keys [lib path]} locals
                            :let [github? (v/parse-github-lib lib)]]
                      (if github?
                        (let [dir-name (v/lib-artifact-id lib)
                              tag-result (resolve-lib-tags root-dir lib dir-name)]
                          (if (r/ok? tag-result)
                            (let [{:keys [tag sha sha-short]} (:ok tag-result)
                                  use-sha (or sha-short sha)]
                              (swap! updated-content v/replace-local-with-git lib tag use-sha)
                              (swap! replaced inc)
                              (println (str "  " (c :cyan (str lib))
                                            " -> " (c :green tag) " " (c :dim use-sha))))
                            (println (c :yellow (str "  Could not resolve " lib
                                                     " — remove :local/root manually")))))
                        ;; Not io.github.* — try local sibling dir first, then mvn
                        (let [sibling-dir (v/infer-sibling-dir path)
                              local-dir (when sibling-dir (fs/path root-dir sibling-dir))
                              local-tag (when (and local-dir (fs/directory? (fs/path local-dir ".git")))
                                          (resolve-local-tags (str local-dir)))]
                          (if-let [latest (and (r/ok? local-tag) (v/latest-tag (:ok local-tag)))]
                            (let [{:keys [tag sha]} latest
                                  use-sha (if (<= (count sha) 12) sha (subs sha 0 7))
                                  canonical (when org (symbol (str "io.github." org "/" sibling-dir)))]
                              (swap! updated-content v/replace-local-with-git lib tag use-sha canonical)
                              (swap! replaced inc)
                              (println (str "  " (c :cyan (str lib))
                                            (when canonical (str " -> " (c :cyan (str canonical))))
                                            " -> " (c :green tag) " " (c :dim use-sha)
                                            " (local sibling: " sibling-dir ")")))
                            (let [mvn-result (resolve-mvn-latest lib false)]
                              (if (r/ok? mvn-result)
                                (let [version (:ok mvn-result)]
                                  (swap! updated-content v/replace-local-with-mvn lib version)
                                  (swap! replaced inc)
                                  (println (str "  " (c :cyan (str lib))
                                                " -> " (c :green version))))
                                (println (c :yellow (str "  Could not resolve " lib
                                                         " — remove :local/root manually")))))))))

                    (when (pos? @replaced)
                      (spit file-path @updated-content)
                      (println (c :green (str "  Updated " (str (fs/relativize root-dir file-path))))))))

                (println)
                (println (c :green "Done. Review the changes and commit."))))

            (println (c :dim "  Pass --fix to auto-split into local.deps.edn and resolve remote coords."))))))))

(defn bump-cmd
  "Bump VERSION file, git commit + tag + push, optionally sync downstream."
  [{:keys [opts]}]
  (let [{:keys [root major minor stable sync org]
         :or {root "."}} opts
        project-dir (str (fs/canonicalize root))
        version-file (str (fs/path project-dir "VERSION"))]

    (when-not (fs/exists? version-file)
      (println (c :red (str "Error: VERSION file not found at " version-file)))
      (System/exit 1))

    (let [current-str  (str/trim (slurp version-file))
          current      (v/parse-semver current-str)]

      (when-not current
        (println (c :red (str "Error: Cannot parse version '" current-str "'")))
        (System/exit 1))

      (let [bump-fn      (cond stable v/bump-major
                               major  v/bump-minor
                               minor  v/bump-patch
                               :else  v/bump-patch)
            new-semver   (bump-fn current)
            new-version  (v/semver->version new-semver)
            new-tag      (v/semver->tag new-semver)]

        (println (c :bold (str "Bumping " current-str " -> " new-version)))
        (println)

        (spit version-file (str new-version "\n"))
        (println (c :green (str "  Updated VERSION: " new-version)))

        (let [extra-version-files (->> (fs/glob project-dir "**/VERSION")
                                       (map str)
                                       (remove #{version-file}))]
          (doseq [f extra-version-files]
            (spit f (str new-version "\n"))
            (println (c :green (str "  Updated " (str (fs/relativize project-dir f)))))))

        (let [git (fn [& args]
                    (let [result (proc/sh (into ["git" "-C" project-dir] args))]
                      (when-not (zero? (:exit result))
                        (println (c :yellow (str "  git " (first args) ": " (str/trim (:err result ""))))))
                      result))
              all-version-files (into ["VERSION"]
                                      (->> (fs/glob project-dir "**/VERSION")
                                           (map #(str (fs/relativize project-dir %)))))]
          (doseq [f all-version-files]
            (git "add" f))
          (git "commit" "-m" (str "release: " new-tag))
          (println (c :green (str "  Committed: release: " new-tag)))

          (git "tag" new-tag)
          (println (c :green (str "  Tagged: " new-tag)))

          (git "push")
          (git "push" "--tags")
          (println (c :green "  Pushed to remote")))

        (println)

        (when (and sync org)
          (println (c :bold "Running sync..."))
          (sync-cmd {:opts {:root (str (fs/parent project-dir))
                            :org org :apply true}}))

        (println (c :green (str "Done: " new-tag)))))))

;; =============================================================================
;; Transitive dependency resolution (v0.5.0)
;; =============================================================================

(defn- fetch-pom-xml
  "Fetch POM XML from a URL. Returns Result<string>."
  [url]
  (r/try-effect*
   :io/fetch-pom
   (let [resp (gate/gate-run http-gate (fn [] (http/get url {:throw false})))]
     (if (= 200 (:status resp))
       (:body resp)
       (throw (ex-info "POM not found" {:url url}))))))

(defn fetch-pom-deps
  "Fetch and parse POM for a Maven artifact. Tries Clojars then Maven Central.
   Returns Result<[{:lib :version}]>."
  [group-id artifact-id version]
  (let [[clojars-url maven-url] (v/pom-urls group-id artifact-id version)
        pom-xml (let [r1 (fetch-pom-xml clojars-url)]
                  (if (r/ok? r1) r1 (fetch-pom-xml maven-url)))]
    (r/ok-> pom-xml v/parse-pom-deps)))

(defn- fetch-git-deps-edn
  "Fetch raw deps.edn content from GitHub. Returns Result<string>."
  [org repo tag]
  (r/try-effect*
   :io/fetch-git-deps
   (let [url (format "https://raw.githubusercontent.com/%s/%s/%s/deps.edn" org repo tag)
         resp (gate/gate-run http-gate (fn [] (http/get url {:throw false})))]
     (if (= 200 (:status resp))
       (:body resp)
       (throw (ex-info "deps.edn not found" {:org org :repo repo :tag tag}))))))

(defn fetch-git-dep-coords
  "Fetch deps.edn from GitHub raw content for a git dep.
   Returns Result<[{:lib :version :type}]>."
  [org repo tag]
  (r/ok-> (fetch-git-deps-edn org repo tag)
          v/deps-edn->dep-coords))

(defn resolve-dep-children
  "Resolve children for a dep. Dispatches by lib type. Uses bounded cache.
   Returns [{:lib :version :type}]."
  [cache lib version]
  (let [key [lib version]]
    (if-let [cached (ba/bget cache key)]
      cached
      (let [lib-str (str lib)
            [group artifact] (str/split lib-str #"/" 2)
            group (or group artifact)
            artifact (or artifact group)
            result (let [resolved (if-let [{:keys [org repo]} (v/parse-github-lib lib)]
                                    (fetch-git-dep-coords org repo version)
                                    (fetch-pom-deps group artifact version))]
                     (if (r/ok? resolved)
                       (mapv #(assoc % :type (or (:type %) :mvn)) (:ok resolved))
                       []))]
        (ba/bput! cache key result)
        result))))

(defn tree-cmd
  "Show transitive dependency tree with conflict detection."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs depth tree-depth conflicts-only]
         :or {root "." depth default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   default-skip-dirs)
        dep-files (find-dep-files {:root root :skip-dirs skip-set :depth depth})
        cache (ba/bounded-atom {:max-entries 500})]

    (println (c :bold "Building dependency tree..."))
    (println)

    (doseq [{:keys [path project] :as dep-file} dep-files
            :let [content (slurp path)
                  mvn-deps (extract-mvn-deps dep-file content)
                  git-deps (if (shadow-deps-file? dep-file)
                             []
                             (v/find-git-deps content))]]

      (let [direct-deps (vec (concat
                              (mapv (fn [{:keys [lib version]}]
                                      {:lib lib :version version :type :mvn})
                                    mvn-deps)
                              (mapv (fn [{:keys [lib tag]}]
                                      {:lib lib :version tag :type :git})
                                    git-deps)))
            resolve-fn (fn [lib version]
                         (resolve-dep-children cache lib version))
            tree (v/build-dep-tree direct-deps resolve-fn tree-depth)
            conflicts (v/find-conflicts tree)]

        (when (or (not conflicts-only) (seq conflicts))
          (println (c :bold (c :cyan project))
                   (c :dim (str " (" (str (fs/relativize root-dir path)) ")")))

          (when-not conflicts-only
            (let [lines (v/format-dep-tree tree conflicts)]
              (doseq [line lines] (println line))))

          (when (seq conflicts)
            (when-not conflicts-only (println))
            (println (c :yellow (str "  " (count conflicts) " conflict(s):")))
            (doseq [[lib versions] (sort-by (comp str key) conflicts)]
              (println (str "    " (c :yellow (str lib)) " — "
                           (str/join " vs " (sort v/version-compare (seq versions)))))))
          (println))))))

;; Workspace automation extracted to bb-depsolve.wave (v0.7.0 refactor)
