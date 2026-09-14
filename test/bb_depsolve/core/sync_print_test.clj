(ns bb-depsolve.core.sync-print-test
  "Tests for the sync mismatch table display: each row names its dep file."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn- print-changes! [changes]
  ((requiring-resolve 'bb-depsolve.core.sync/print-changes!) changes))

(deftest mismatch-lines-name-their-dep-file
  (let [row (fn [path] {:coord :git :project "bb-depsolve" :lib 'io.github.hive-agi/hive-test
                        :old-tag "v0.4.3" :old-sha "aaaaaaa" :new-tag "v0.4.4" :new-sha "bbbbbbb" :path path})
        out (with-out-str (print-changes! [(row "/w/bb-depsolve/deps.edn") (row "/w/bb-depsolve/bb.edn")]))
        lines (filter #(str/includes? % "hive-test") (str/split-lines out))]
    (is (= 2 (count lines)))
    (is (some #(str/includes? % "deps.edn") lines))
    (is (some #(str/includes? % "bb.edn") lines))
    (is (apply distinct? lines))))

(deftest mvn-row-names-its-file
  (let [row (fn [path] {:coord :mvn :project "bb-depsolve" :lib 'io.github.hive-agi/hive-dsl
                        :old-version "0.5.23" :new-version "0.5.24" :source nil :unreachable nil :path path})
        out (with-out-str (print-changes! [(row "/w/bb-depsolve/deps.edn")]))
        lines (filter #(str/includes? % "hive-dsl") (str/split-lines out))]
    (is (= 1 (count lines)))
    (is (some #(str/includes? % "deps.edn") lines))))

(deftest missing-path-prints-dash
  (let [row {:coord :git :project "bb-depsolve" :lib 'io.github.hive-agi/hive-test
             :old-tag "v0.4.3" :old-sha "aaaaaaa" :new-tag "v0.4.4" :new-sha "bbbbbbb" :path nil}
        out (with-out-str (print-changes! [row]))]
    (is (str/includes? out "-"))))
