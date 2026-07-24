(ns bb-depsolve.version.parse-property-test
  "Property tests for dependency extraction."
  (:require [bb-depsolve.version :as v]
            [bb-depsolve.version.generators :as g]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.properties :as props]))

(props/defprop-total p1-find-git-deps-totality
  v/find-git-deps gen/string-alphanumeric)

(props/defprop-total p1-find-mvn-deps-totality
  v/find-mvn-deps gen/string-alphanumeric)

(props/defprop-total p12-find-local-deps-totality
  v/find-local-deps gen/string-alphanumeric)

(defspec p12-find-local-deps-roundtrip 200
  (prop/for-all [{:keys [content lib path]} g/gen-local-dep-content]
                (let [deps (v/find-local-deps content)]
                  (and (= 1 (count deps))
                       (= lib (:lib (first deps)))
                       (= path (:path (first deps)))))))

(props/defprop-total p23-deps-edn-dep-coords-totality
  v/deps-edn->dep-coords gen/string-alphanumeric)
