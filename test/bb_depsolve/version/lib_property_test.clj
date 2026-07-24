(ns bb-depsolve.version.lib-property-test
  "Property tests for lib identity and coordinate naming."
  (:require [bb-depsolve.version :as v]
            [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defspec p10-parse-github-lib-roundtrip 200
  (prop/for-all [org (gen/elements ["hive-agi" "clojure" "babashka"])
                 repo (gen/elements ["core" "fs" "process" "hive-events"])]
                (let [lib (symbol (str "io.github." org "/" repo))
                      parsed (v/parse-github-lib lib)]
                  (and (= org (:org parsed))
                       (= repo (:repo parsed))))))

(defspec p21-group-id-path-dots-to-slashes 200
  (prop/for-all [group (gen/elements ["com.fasterxml" "org.clojure" "cheshire" "a.b.c.d"])]
                (let [path (v/group-id->path group)]
                  (and (string? path)
                       (not (str/includes? path "."))
                       (= (count (filter #(= % \/) path))
                          (count (filter #(= % \.) group)))))))
