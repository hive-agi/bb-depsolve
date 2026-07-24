(ns bb-depsolve.registry
  "IArtifactRegistry over git tags and published maven versions."
  (:require [bb-depsolve.core :as core]
            [bb-depsolve.port :as port]
            [bb-depsolve.version :as v]
            [hive-dsl.result :as r]))

(defn tag-versions
  "Semver tags published for LIB, in maven form. Empty when unreachable."
  [lib]
  (if-let [{:keys [org repo]} (v/parse-github-lib lib)]
    (let [result (core/resolve-remote-tags org repo)]
      (if (r/ok? result)
        (into #{}
              (comp (map :tag)
                    (filter #(re-matches #"^v\d+\.\d+\.\d+$" %))
                    (map v/tag->mvn-version))
              (:ok result))
        #{}))
    #{}))

(defn mvn-version
  "Latest version of LIB across the maven registries, or nil."
  [lib allow-pre?]
  (let [result (core/resolve-mvn-latest lib allow-pre?)]
    (when (r/ok? result) (:ok result))))

(defn known-versions
  "Versions of LIB observable from any source."
  [lib allow-pre?]
  (let [mvn (mvn-version lib allow-pre?)]
    (cond-> (tag-versions lib)
      mvn (conj mvn))))

(defrecord LiveRegistry [opts]
  port/IArtifactRegistry
  (published? [_ lib version]
    (let [want (v/tag->mvn-version version)
          known (known-versions lib (:allow-pre? opts))
          highest (last (sort v/version-compare known))]
      (r/ok (boolean (or (contains? known want)
                         (and highest (not (v/version-newer? highest want))))))))

  (latest-version [_ lib]
    (r/ok (last (sort v/version-compare (known-versions lib (:allow-pre? opts)))))))

(defn live-registry
  "IArtifactRegistry backed by GitHub tags, Clojars, Maven Central and — when
   MAVEN_URL is set — the private Gitea Maven registry.

   OPTS: :allow-pre? include pre-release versions (default false)."
  ([] (live-registry {}))
  ([opts] (->LiveRegistry opts)))
