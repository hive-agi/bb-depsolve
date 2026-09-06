(ns bb-depsolve.registry.live
  "IArtifactRegistry over git tags and published maven versions."
  (:require [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.release.port :as port]
            [bb-depsolve.version.api :as v]
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

(defn registry-versions
  "Every Maven artifact version of LIB, one entry per (registry, version),
   each saying which consumers can reach it."
  [lib allow-pre?]
  (vec (for [{:keys [id url public? versions]} (registries/resolve-mvn-versions-by-registry lib allow-pre?)
             version (sort v/version-compare versions)]
         {:id id :url url :public? public? :kind :mvn :version version})))

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
    (r/ok (last (sort v/version-compare (known-versions lib (:allow-pre? opts))))))

  (published-versions [_ lib]
    (r/ok (into (mapv (fn [version] {:id "git" :public? true :kind :git :version version})
                      (sort v/version-compare (tag-versions lib)))
                (registry-versions lib (:allow-pre? opts))))))

(defn live-registry
  "IArtifactRegistry backed by GitHub tags, Clojars, Maven Central and the
   private Maven registry the workspace declares. `published-versions` keeps
   the sources apart so an await can ask whether each consumer can actually
   fetch the artifact.

   OPTS: :allow-pre? include pre-release versions (default false)"
  ([] (live-registry {}))
  ([opts] (->LiveRegistry opts)))
