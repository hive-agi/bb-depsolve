(ns bb-depsolve.core.resolver
  "Version resolution port for the tree and upgrade commands.

   registry-version {:id :url :public? :version}
   dep-coord        {:lib :version :type}"
  (:require [bb-depsolve.version.api :as v]
            [hive-dsl.result :as r]))

(defprotocol IVersionResolver
  "Remote version observations. Each method returns a Result."
  (latest-by-registry [this lib allow-pre?]
    "=> Result of [registry-version]: the latest version of LIB each registry
     that answered lists, one entry per registry. Errs when none answered or
     none lists an acceptable version.")
  (dep-children [this lib version]
    "=> Result of [dep-coord]: the direct dependencies of LIB at VERSION."))

(defn latest-of
  "The highest :version among REGISTRY-VERSIONS, or nil when empty. Pure."
  [registry-versions]
  (:version (last (sort-by :version v/version-compare registry-versions))))

(defn resolve-latest
  "=> Result of the highest version of LIB any registry RESOLVER consults
   lists. SELECT picks the version from the [registry-version] rows
   (default `latest-of`)."
  ([resolver lib allow-pre?]
   (resolve-latest resolver lib allow-pre? latest-of))
  ([resolver lib allow-pre? select]
   (r/let-ok [rows (latest-by-registry resolver lib allow-pre?)]
     (r/ok (select rows)))))

(defn children-or-empty
  "The dep-coords RESOLVER reports for LIB at VERSION; [] when it errs."
  [resolver lib version]
  (let [result (dep-children resolver lib version)]
    (if (r/ok? result) (vec (:ok result)) [])))

;; =============================================================================
;; In-memory adapter
;; =============================================================================

(defrecord MemoryResolver [latest children calls]
  IVersionResolver
  (latest-by-registry [_ lib allow-pre?]
    (swap! calls conj [:latest-by-registry lib allow-pre?])
    (if-let [rows (get latest lib)]
      (r/ok (vec rows))
      (r/err :io/no-published-version {:lib lib :allow-pre? allow-pre?})))

  (dep-children [_ lib version]
    (swap! calls conj [:dep-children lib version])
    (if-let [coords (get children [lib version])]
      (r/ok (vec coords))
      (r/err :io/fetch-pom {:lib lib :version version}))))

(defn memory-resolver
  "In-memory IVersionResolver.

   OPTS:
     :latest    {lib [registry-version]}  a lib absent here errs
     :children  {[lib version] [dep-coord]}  a pair absent here errs

   Every call is recorded, in order, at @(:calls resolver)."
  ([] (memory-resolver {}))
  ([{:keys [latest children]}]
   (->MemoryResolver (or latest {}) (or children {}) (atom []))))
