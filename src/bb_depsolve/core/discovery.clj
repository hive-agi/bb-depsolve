(ns bb-depsolve.core.discovery
  "Workspace scanning: dep-file discovery and project layout."
  (:require [babashka.fs :as fs]
            [bb-depsolve.version :as v]
            [clojure.string :as str]))

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
