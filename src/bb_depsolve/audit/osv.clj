(ns bb-depsolve.audit.osv
  "CVE audit via OSV.dev (Maven ecosystem).

   Layer 2 (Action): HTTP I/O to OSV.dev, orchestrates Maven dep collection
   (direct + transitive) and vulnerability reporting.

   Pure vulnerability parsing / sorting lives in `bb-depsolve.version.api`.
   Dep-file discovery / transitive resolution lives in `bb-depsolve.core.api`."
  (:require [babashka.http-client :as http]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.version.api :as v]
            [cheshire.core :as json]
            [clojure.string :as str]
            [hive-dsl.bounded-atom :as ba]
            [hive-dsl.result :as r]
            [hive-weave.parallel :as par]))

(def ^:private osv-concurrency
  "Simultaneous OSV.dev queries. This is the rate limit we hold ourselves to."
  5)

(def ^:private osv-timeout-ms 30000)

(defn- query-osv
  "Query OSV.dev for vulnerabilities affecting a Maven package at a version.
   Returns Result<[parsed-vuln-map ...]>. Concurrency is bounded by the caller
   (query-osv-batch), not here."
  [group-id artifact-id version]
  (r/try-effect*
   :io/osv-query
   (let [pkg-name (str group-id ":" artifact-id)
         resp (http/post "https://api.osv.dev/v1/query"
                         {:headers {"Content-Type" "application/json"}
                          :body (json/generate-string
                                  {:package {:name pkg-name
                                             :ecosystem "Maven"}
                                   :version version})
                          :throw false})]
     (if (= 200 (:status resp))
       (let [body (json/parse-string (:body resp) true)
             vulns (get body :vulns [])]
         (->> vulns
              (keep v/parse-osv-vuln)
              (v/sort-vulns-by-severity)
              (vec)))
       []))))

(defn- query-osv-batch
  "Query OSV.dev for a batch of Maven deps, at most `osv-concurrency` at a time.
   Returns map of {:lib {:version :vulns [...]}}, carrying only deps that have
   findings. A query that times out or throws contributes nothing."
  [deps]
  (let [targets (into []
                      (keep (fn [{:keys [lib version]}]
                              (let [[group artifact] (v/lib->maven-coord lib)]
                                (when (and group artifact version)
                                  {:lib lib :group group :artifact artifact
                                   :version version}))))
                      deps)
        results (par/bounded-pmap
                 {:concurrency osv-concurrency
                  :timeout-ms  osv-timeout-ms
                  :fallback    nil}
                 (fn [{:keys [group artifact version]}]
                   (query-osv group artifact version))
                 targets)]
    (into {}
          (keep (fn [[{:keys [lib version]} result]]
                  (when (and result (r/ok? result) (seq (:ok result)))
                    [lib {:version version :vulns (:ok result)}])))
          (map vector targets results))))

(defn- collect-all-mvn-deps
  "Collect all Maven deps (direct + transitive) from dep files.
   Returns deduped vec of {:lib :version}."
  [dep-files cache tree-depth]
  (let [seen (atom #{})]
    (->> dep-files
         (mapcat (fn [{:keys [path] :as dep-file}]
                   (let [content (slurp path)
                         mvn-deps (discovery/extract-mvn-deps dep-file content)
                         git-deps (if (discovery/shadow-deps-file? dep-file)
                                    []
                                    (v/find-git-deps content))
                         direct (vec (concat
                                      (mapv (fn [{:keys [lib version]}]
                                              {:lib lib :version version :type :mvn})
                                            mvn-deps)
                                      (mapv (fn [{:keys [lib tag]}]
                                              {:lib lib :version tag :type :git})
                                            git-deps)))]
                     (if tree-depth
                       ;; Resolve transitives
                       (let [resolve-fn (fn [lib version]
                                          (resolve/resolve-dep-children cache lib version))
                             tree (v/build-dep-tree direct resolve-fn tree-depth)]
                         (letfn [(flatten-tree [nodes]
                                   (mapcat (fn [{:keys [lib version type children]}]
                                             (cons {:lib lib :version version :type type}
                                                   (flatten-tree children)))
                                           nodes))]
                           (flatten-tree tree)))
                       direct))))
         (filter #(= :mvn (:type %)))
         (filter (fn [{:keys [lib version]}]
                   (let [k [lib version]]
                     (when-not (contains? @seen k)
                       (swap! seen conj k)
                       true))))
         (vec))))

(defn audit-cmd
  "Scan dependencies for known CVEs via OSV.dev."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs depth tree-depth]
         :or {root "." depth discovery/default-depth}} opts
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        dep-files (discovery/find-dep-files {:root root :skip-dirs skip-set :depth depth})
        cache (ba/bounded-atom {:max-entries 500})]

    (println (ui/c :bold "Scanning dependencies for known CVEs (via OSV.dev)..."))
    (println)

    (let [all-deps (collect-all-mvn-deps dep-files cache tree-depth)
          _ (println (str "  Found " (ui/c :cyan (str (count all-deps)))
                          " unique Maven dependencies"
                          (when tree-depth " (including transitives)")))
          _ (println (str "  Querying OSV.dev..."))
          _ (println)
          vulnerable (query-osv-batch all-deps)
          total-vulns (reduce + 0 (map (comp count :vulns) (vals vulnerable)))]

      (if (empty? vulnerable)
        (println (ui/c :green "No known vulnerabilities found."))
        (do
          (println (ui/c :bold (ui/c :red (str total-vulns " vulnerabilit"
                                                   (if (= 1 total-vulns) "y" "ies")
                                                   " found in " (count vulnerable)
                                                   " package" (when (> (count vulnerable) 1) "s")
                                                   ":"))))
          (println)
          (doseq [[lib {:keys [version vulns]}] (sort-by key vulnerable)]
            (println (str "  " (ui/c :bold (str lib)) " " (ui/c :dim version)))
            (doseq [{:keys [id severity summary fixed-in]} vulns]
              (let [sev-color (case severity
                                "CRITICAL" :red
                                "HIGH"     :red
                                "MODERATE" :yellow
                                "MEDIUM"   :yellow
                                :dim)
                    fix-str (when (seq fixed-in)
                              (str " → fix: " (str/join ", " fixed-in)))]
                (println (str "    " (ui/c sev-color (or severity "?"))
                              " " (ui/c :bold id)
                              "  " (or summary "")
                              (when fix-str (ui/c :green fix-str))))))
            (println))

          (println (ui/c :dim "Source: https://osv.dev"))
          (println)

          ;; Exit with non-zero for CI integration
          (System/exit 1))))))
