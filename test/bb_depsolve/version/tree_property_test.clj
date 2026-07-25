(ns bb-depsolve.version.tree-property-test
  "Property tests for dependency trees."
  (:require [bb-depsolve.version.api :as v]
            [bb-depsolve.version.generators :as g]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defspec p24-find-conflicts-only-multi-version 100
  (prop/for-all [libs (gen/vector (gen/tuple g/gen-lib-sym g/gen-version-string) 1 10)]
                (let [tree (mapv (fn [[lib ver]]
                                  {:lib lib :version ver :children []})
                                libs)
                      conflicts (v/find-conflicts tree)]
                  (every? (fn [[_ vs]] (> (count vs) 1)) conflicts))))
