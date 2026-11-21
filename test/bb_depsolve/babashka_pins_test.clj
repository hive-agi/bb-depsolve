(ns bb-depsolve.babashka-pins-test
  "Floors on the dependencies the bb-depsolve CLI loads under babashka.

   hive-weave 0.4.0's pool.clj names java.util.concurrent.RejectedExecutionException,
   a class babashka 1.13 does not expose, so the CLI dies at load. 0.4.2 is
   the first release that loads under babashka again."
  (:require [bb-depsolve.version.api :as v]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]))

(def floors
  {'io.github.hive-agi/hive-weave "0.4.2"})

(defn- pinned [file lib]
  (get-in (edn/read-string (slurp file)) [:deps lib :mvn/version]))

(deftest the-cli-pins-load-under-babashka-test
  (doseq [file ["bb.edn" "deps.edn"]
          [lib floor] floors
          :let [version (pinned file lib)]]
    (testing (str file " " lib)
      (is (some? version) "pinned by :mvn/version")
      (is (not (v/version-newer? version floor))
          (str version " is below " floor ", which babashka cannot load")))))
