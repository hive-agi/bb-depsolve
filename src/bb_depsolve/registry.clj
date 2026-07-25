(ns bb-depsolve.registry
  "IArtifactRegistry over git tags and published maven versions."
  (:require [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.port :as port]
            [bb-depsolve.version :as v]
            [hive-dsl.result :as r]
            [bb-depsolve.core.resolve.registries :as registries]))

(defn tag-versions
  "Semver tags published for LIB, in maven form. Empty when unreachable."
  [lib]
  (if-let [{:keys [org repo]} (v/parse-github-lib lib)]
    (let [result (resolve/resolve-remote-tags org repo)]
      (if (r/ok? result)
        (into #{}
              (comp (map :tag)
                    (filter #(re-matches #"^v\d+\.\d+\.\d+$" %))
                    (map v/tag->mvn-version))
              (:ok result))
        #{}))
    #{}))

(defn mvn-versions
  "Every version of LIB the maven registries list."
  [lib allow-pre?]
  (registries/resolve-mvn-versions lib allow-pre?))

(defn known-versions
  "Versions of LIB observable from any source: semver git tags plus every
   version the maven registries enumerate."
  [lib allow-pre?]
  (into (tag-versions lib) (mvn-versions lib allow-pre?)))

(defrecord LiveRegistry [opts]
  port/IArtifactRegistry
  (published? [_ lib version]
    (r/ok (contains? (known-versions lib (:allow-pre? opts))
                     (v/tag->mvn-version version))))

  (latest-version [_ lib]
    (r/ok (last (sort v/version-compare (known-versions lib (:allow-pre? opts)))))))

(defn live-registry
  "IArtifactRegistry backed by GitHub tags, Clojars, Maven Central and — when
   MAVEN_URL is set — the private Gitea Maven registry.

   OPTS: :allow-pre? include pre-release versions (default false)."
  ([] (live-registry {}))
  ([opts] (->LiveRegistry opts)))
