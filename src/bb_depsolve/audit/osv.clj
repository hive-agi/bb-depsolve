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
            [hive-weave.parallel :as par]
            [bb-depsolve.core.upgrade :as upgrade]
            [babashka.fs :as fs]))

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

(def ^:private osv-batch-size
  "Packages per /v1/querybatch request. OSV documents batch queries as the
   supported way to ask about many packages at once."
  1000)

(defn- query-osv-batch-ids
  "POST /v1/querybatch for one batch of targets.
   Returns Result<[[vuln-id ...] ...]> — one entry per target, in the SAME
   order as the request, since the API answers positionally."
  [targets]
  (r/try-effect*
   :io/osv-querybatch
   (let [queries (mapv (fn [{:keys [group artifact version]}]
                         {:package {:name (str group ":" artifact)
                                    :ecosystem "Maven"}
                          :version version})
                       targets)
         resp (http/post "https://api.osv.dev/v1/querybatch"
                         {:headers {"Content-Type" "application/json"}
                          :body (json/generate-string {:queries queries})
                          :throw false})]
     (if (= 200 (:status resp))
       (let [results (:results (json/parse-string (:body resp) true))]
         (mapv (fn [r] (mapv :id (get r :vulns []))) results))
       (throw (ex-info "OSV querybatch HTTP error" {:status (:status resp)}))))))

(defn- fetch-osv-vuln
  "GET /v1/vulns/{id} — the full record for one vulnerability.
   Returns Result<parsed-vuln-map>; nil-ok inside the Result when unparseable.
   querybatch answers with IDs only, so the details come from here."
  [id]
  (r/try-effect*
   :io/osv-vuln
   (let [resp (http/get (str "https://api.osv.dev/v1/vulns/" id) {:throw false})]
     (if (= 200 (:status resp))
       (v/parse-osv-vuln (json/parse-string (:body resp) true))
       (throw (ex-info "OSV vuln HTTP error" {:status (:status resp) :id id}))))))

(defn- query-osv-batch
  "Query OSV.dev for a batch of Maven deps.
   Returns map of {:lib {:version :sites :vulns [...]}}, carrying only deps that
   have findings.

   Uses POST /v1/querybatch — one request per `osv-batch-size` packages, whose
   results are positional and carry vulnerability IDs only — then GET
   /v1/vulns/{id} once per DISTINCT id for the full record (severity, CVSS,
   fixed-version events, advisory references). For a workspace-sized scan that
   is a handful of requests instead of one per dependency. Falls back to the
   per-dep /v1/query path when a batch request fails, so a batch outage
   degrades coverage rather than losing it."
  [deps]
  (let [targets (into []
                      (keep (fn [{:keys [lib version sites]}]
                              (let [[group artifact] (v/lib->maven-coord lib)]
                                (when (and group artifact version)
                                  {:lib lib :group group :artifact artifact
                                   :version version :sites (or sites #{})}))))
                      deps)
        batches (partition-all osv-batch-size targets)
        id-lists (into []
                       (mapcat (fn [batch]
                                 (let [result (query-osv-batch-ids batch)]
                                   (if (r/ok? result)
                                     (:ok result)
                                     (repeat (count batch) ::batch-failed)))))
                       batches)
        ids (->> id-lists (remove #{::batch-failed}) (apply concat) (distinct) (vec))
        details (into {}
                      (keep (fn [[id result]]
                              (when (and result (r/ok? result) (:ok result))
                                [id (:ok result)])))
                      (map vector ids
                           (par/bounded-pmap
                            {:concurrency osv-concurrency
                             :timeout-ms  osv-timeout-ms
                             :fallback    nil}
                            fetch-osv-vuln
                            ids)))
        fallback-targets (into [] (keep (fn [[t id-list]]
                                          (when (= ::batch-failed id-list) t))
                                        (map vector targets id-lists)))
        fallback-vulns (into {}
                             (keep (fn [[{:keys [lib]} result]]
                                     (when (and result (r/ok? result) (seq (:ok result)))
                                       [lib (:ok result)])))
                             (map vector fallback-targets
                                  (par/bounded-pmap
                                   {:concurrency osv-concurrency
                                    :timeout-ms  osv-timeout-ms
                                    :fallback    nil}
                                   (fn [{:keys [group artifact version]}]
                                     (query-osv group artifact version))
                                   fallback-targets)))]
    (into {}
          (keep (fn [[{:keys [lib version sites]} id-list]]
                  (let [vulns (if (= ::batch-failed id-list)
                                (get fallback-vulns lib)
                                (->> id-list (keep details) (vec)))]
                    (when (seq vulns)
                      [lib {:version version
                            :sites sites
                            :vulns (vec (v/sort-vulns-by-severity vulns))}]))))
          (map vector targets id-lists))))

(defn- collect-all-mvn-deps
  "Collect all Maven deps (direct + transitive) from dep files.
   Returns deduped vec of {:lib :version :sites}.

   :sites is the set of {:project :path} dep files that DECLARE the coordinate;
   it is empty for a transitive, which is why a transitive finding cannot be
   fixed by editing a version in place. Dedup is by [lib version], merging the
   sites of every occurrence."
  [dep-files cache tree-depth]
  (let [order (atom [])
        by-coord (atom {})]
    (doseq [{:keys [path project] :as dep-file} dep-files
            :let [content (slurp path)
                  mvn-deps (discovery/extract-mvn-deps dep-file content)
                  git-deps (if (discovery/shadow-deps-file? dep-file)
                             []
                             (v/find-git-deps content))
                  site {:project project :path path}
                  direct (vec (concat
                               (mapv (fn [{:keys [lib version]}]
                                       {:lib lib :version version :type :mvn :sites #{site}})
                                     mvn-deps)
                               (mapv (fn [{:keys [lib tag]}]
                                       {:lib lib :version tag :type :git :sites #{site}})
                                     git-deps)))
                  entries (if tree-depth
                            (let [resolve-fn (fn [lib version]
                                               (resolve/resolve-dep-children cache lib version))
                                  tree (v/build-dep-tree direct resolve-fn tree-depth)]
                              (letfn [(flatten-tree [nodes]
                                        (mapcat (fn [{:keys [lib version type children sites]}]
                                                  (cons {:lib lib :version version :type type
                                                         :sites (or sites #{})}
                                                        (flatten-tree children)))
                                                nodes))]
                                (flatten-tree tree)))
                            direct)]]
      (doseq [{:keys [lib version type sites]} entries
              :when (= :mvn type)
              :let [k [lib version]]]
        (when-not (contains? @by-coord k)
          (swap! order conj k))
        (swap! by-coord update k
               (fn [existing]
                 (-> (or existing {:lib lib :version version :sites #{}})
                     (update :sites into (or sites #{})))))))
    (mapv @by-coord @order)))

(defn audit-cmd
  "Scan dependencies for known CVEs via OSV.dev.

   Reports WHERE each vulnerable coordinate is declared, its CVSS vector and
   advisory, and the version that clears every finding against it. With --fix,
   writes those versions into the declaring dep files (same writer as
   `upgrade`), which is only possible for DIRECT deps — a transitive is
   reported with the dep file that pulls it and left alone.

   Exits 1 when anything is still vulnerable after the run, for CI."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs depth tree-depth fix]
         :or {root "." depth discovery/default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        dep-files (discovery/find-dep-files {:root root :skip-dirs skip-set :depth depth})
        dep-file-index (into {} (map (fn [df] [(:path df) df])) dep-files)
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
        (let [findings (->> vulnerable
                            (map (fn [[lib {:keys [version sites vulns]}]]
                                   {:lib lib :version version :sites sites :vulns vulns
                                    :fix (v/recommended-fix version vulns)}))
                            (sort-by (comp str :lib)))]
          (println (ui/c :bold (ui/c :red (str total-vulns " vulnerabilit"
                                               (if (= 1 total-vulns) "y" "ies")
                                               " found in " (count vulnerable)
                                               " package" (when (> (count vulnerable) 1) "s")
                                               ":"))))
          (println)

          (doseq [{:keys [lib version sites vulns fix]} findings]
            (println (str "  " (ui/c :bold (str lib)) " " (ui/c :dim version)))
            (if (seq sites)
              (doseq [{:keys [project path]} (sort-by :path sites)]
                (println (ui/c :dim (str "    declared in " project " (" (fs/relativize root-dir path) ")"))))
              (println (ui/c :dim "    transitive — no dep file declares it directly")))
            (doseq [{:keys [id severity cvss summary fixed-in references]} vulns]
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
                              (when fix-str (ui/c :green fix-str))))
                (when cvss
                  (println (ui/c :dim (str "        " cvss))))
                (when-let [url (first references)]
                  (println (ui/c :dim (str "        " url))))))
            (if fix
              (println (ui/c :green (str "    ⇒ clears every finding: " version " -> " fix)))
              (println (ui/c :yellow "    ⇒ no single version clears every finding — needs manual triage")))
            (println))

          (println (ui/c :dim "Source: https://osv.dev"))
          (println)

          (let [fixable (->> findings
                             (filter :fix)
                             (mapcat (fn [{:keys [lib version sites fix]}]
                                       (map (fn [{:keys [project path]}]
                                              {:path path :project project :lib lib
                                               :old-version version :new-version fix})
                                            sites)))
                             (vec))]
            (cond
              (and fix (seq fixable))
              (do (println (ui/c :bold "Applying fixes..."))
                  (println)
                  (upgrade/apply-mvn-upgrades! root-dir fixable dep-file-index)
                  (println)
                  (println (ui/c :yellow "Re-run `bb-depsolve audit` to confirm, and test before committing.")))

              (and fix (empty? fixable))
              (println (ui/c :yellow "Nothing auto-fixable: every finding is transitive or has no clearing version."))

              (seq fixable)
              (println (ui/c :dim (format "  %d of these are auto-fixable — pass --fix to write the versions above."
                                          (count fixable))))

              :else
              (println (ui/c :dim "  None are auto-fixable (transitive, or no clearing version published).")))
            (println)

            ;; Exit with non-zero for CI integration — unless --fix cleared everything.
            (when-not (and fix (seq fixable) (= (count fixable) (count findings)))
              (System/exit 1))))))))
