(ns bb-depsolve.core.resolve
  "Registry resolution: tags and latest published versions."
  (:require [babashka.fs :as fs]
            [babashka.process :as proc]
            [bb-depsolve.core.auth :as auth]
            [bb-depsolve.core.fetch :as fetch]
            [bb-depsolve.schema.api :as sch]
            [bb-depsolve.version.api :as v]
            [clojure.string :as str]
            [hive-dsl.bounded-atom :as ba]
            [hive-dsl.result :as r]))

(defn resolve-local-tags
  "Resolve all tags from a local git repo. Returns Result<[{:tag :sha :sha-short}]>.
   Annotated tags report the commit behind them, which is what tools.deps
   validates a :git/sha against."
  [repo-dir]
  (r/try-effect*
   :io/git-local-tags
   (let [result (proc/sh ["git" "-C" (str repo-dir) "tag" "--sort=-version:refname"
                          "-l" "v*"
                          "--format=%(refname:short)%09%(*objectname)%09%(objectname)"])]
     (if (zero? (:exit result))
       (v/parse-local-tag-output (:out result))
       (throw (ex-info "git tag failed" {:exit (:exit result)}))))))

(defn resolve-remote-tags
  "Resolve tags from a GitHub remote without cloning.
   Returns Result<[{:tag :sha :sha-short}]>. Annotated tags report their peeled
   commit, which is what tools.deps validates a :git/sha against."
  [org repo]
  (r/try-effect*
   :io/git-ls-remote
   (let [url (format auth/github-url org repo)
         result (proc/sh ["git" "ls-remote" "--tags" "--sort=-version:refname" url])]
     (if (zero? (:exit result))
       (v/parse-ls-remote-tags (:out result))
       (throw (ex-info "git ls-remote failed" {:exit (:exit result)}))))))

(defn resolve-lib-tags
  "Resolve the latest tag+sha for a git lib.
   Uses GitHub remote first, falls back to local clone.
   Returns Result<{:tag :sha :source}>. The resolved value is
   schema-validated at this boundary (fail-loud)."
  [root-dir lib-sym dir-name]
  (if-let [{:keys [org repo]} (v/parse-github-lib lib-sym)]
    (let [remote-result (resolve-remote-tags org repo)]
      (if (and (r/ok? remote-result) (seq (:ok remote-result)))
        (if-let [latest (v/latest-tag (:ok remote-result))]
          (r/ok (assoc (sch/validate! :bb-depsolve/resolved-lib latest) :source :remote))
          (r/err :parse/no-semver-tags {:lib lib-sym}))
        (let [local-dir (fs/path root-dir dir-name)]
          (if (fs/directory? (fs/path local-dir ".git"))
            (r/let-ok [tags (resolve-local-tags local-dir)]
                      (if-let [latest (v/latest-tag tags)]
                        (r/ok (assoc (sch/validate! :bb-depsolve/resolved-lib latest) :source :local))
                        (r/err :parse/no-semver-tags {:lib lib-sym})))
            (r/err :io/not-found {:lib lib-sym :dir (str local-dir)})))))
    (r/err :parse/not-github-lib {:lib lib-sym})))

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
                                    (fetch/fetch-git-dep-coords org repo version)
                                    (fetch/fetch-pom-deps group artifact version))]
                     (if (r/ok? resolved)
                       (mapv #(assoc % :type (or (:type %) :mvn)) (:ok resolved))
                       []))]
        (ba/bput! cache key result)
        result))))