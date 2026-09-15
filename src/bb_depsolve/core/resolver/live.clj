(ns bb-depsolve.core.resolver.live
  "IVersionResolver over the maven registries and published POM / deps.edn
   artifacts."
  (:require [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.core.resolve.registries :as registries]
            [bb-depsolve.core.resolver :as resolver]
            [hive-dsl.bounded-atom :as ba]
            [hive-dsl.result :as r]))

(defrecord LiveResolver [cache]
  resolver/IVersionResolver
  (latest-by-registry [_ lib allow-pre?]
    (registries/resolve-mvn-by-registry lib (boolean allow-pre?)))

  (dep-children [_ lib version]
    (r/ok (resolve/resolve-dep-children cache lib version))))

(defn live-resolver
  "IVersionResolver backed by Clojars, Maven Central, the private Maven
   registry and cached ~/.m2 metadata (latest-by-registry), and by fetched
   POMs / GitHub deps.edn files (dep-children). Children are memoized in a
   bounded cache owned by this resolver.

   OPTS: :max-entries  child-cache bound (default 500)"
  ([] (live-resolver {}))
  ([{:keys [max-entries] :or {max-entries 500}}]
   (->LiveResolver (ba/bounded-atom {:max-entries max-entries}))))
