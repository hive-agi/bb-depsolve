(ns bb-depsolve.wave.deep-lint
  "Deep lint: check the latest tagged release for :local/root pins."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [bb-depsolve.version :as v]
            [bb-depsolve.ui :as ui]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.fetch :as fetch]
            [bb-depsolve.core.resolve :as resolve]))

(defn deep-lint-cmd
  "Deep lint: fetch the latest published tag's deps.edn from the forge
   and check if it still contains :local/root anti-patterns.

   Catches the case where a project was tagged BEFORE bb-depsolve lint --fix
   ran, so consumers still pull a release pinned to local paths.

   Workspace-level: scans every project with a VERSION + tag remote.
   Reports lib + tag + locals found. Exits non-zero when any issues found."
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs org]
         :or {root "."}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        projects (discovery/find-workspace-projects root-dir skip-set)]

    (when-not org
      (println (ui/c :red "Error: --org is required for deep-lint"))
      (System/exit 1))

    (println (ui/c :bold "Deep-lint: scanning latest tagged releases for :local/root..."))
    (println)

    (let [issues (atom 0)
          checked (atom 0)]
      (doseq [project-dir projects
              :let [project (str (fs/file-name project-dir))
                    lib-sym (symbol (str "io.github." org "/" project))
                    tag-r (resolve/resolve-lib-tags root-dir lib-sym project)]]
        (cond
          (not (and (map? tag-r) (contains? tag-r :ok)))
          (println (ui/c :dim (str "  " project " — no resolvable tag, skipped")))

          :else
          (let [{:keys [tag]} (:ok tag-r)
                forge-info (v/parse-forge-lib lib-sym)]
            (if-not forge-info
              (println (ui/c :dim (str "  " project " — non-forge lib, skipped")))
              (let [fetched (fetch/fetch-git-deps-edn (:forge forge-info)
                                                    (:org forge-info)
                                                    (:repo forge-info)
                                                    tag)]
                (swap! checked inc)
                (if-not (and (map? fetched) (contains? fetched :ok))
                  (println (ui/c :dim (str "  " project " — could not fetch deps.edn for " tag)))
                  (let [content (:ok fetched)
                        locals (v/find-local-deps content)]
                    (if (empty? locals)
                      (println (ui/c :green (format "  %s @ %s — clean" project tag)))
                      (do
                        (swap! issues inc)
                        (println (ui/c :yellow (format "  %s @ %s — %d :local/root dep(s)"
                                                          project tag (count locals))))
                        (doseq [{:keys [lib path]} locals]
                          (println (str "      " (ui/c :cyan (str lib))
                                        " -> " (ui/c :yellow path)))))))))))))

      (println)
      (println (ui/c :bold (format "Checked: %d  Issues: %d" @checked @issues)))
      (when (pos? @issues)
        (System/exit 1)))))
