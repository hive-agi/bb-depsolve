(ns bb-depsolve.core.sync-print-test
  "Boundary test: the print-changes! output names its dep files.
   Strips ANSI, then asserts each stripped line contains the exact dep-file
   token surrounded by whitespace."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn- strip-ansi [s]
  (str/replace s #"\u001b\[[0-9;]*m" ""))

(defn- print-changes! [changes]
  ((requiring-resolve 'bb-depsolve.core.sync/print-changes!) changes))

(deftest each-line-names-its-dep-file
  (let [git-row  (fn [path] {:coord :git :project "bb-depsolve" :lib 'io.github.hive-agi/hive-test
                             :old-tag "v0.4.3" :old-sha "aaaaaaa" :new-tag "v0.4.4" :new-sha "bbbbbbb" :path path})
        out (strip-ansi (with-out-str (print-changes! [(git-row "/w/bb-depsolve/deps.edn")
                                                       (git-row "/w/bb-depsolve/bb.edn")])))
        lines (filter #(str/includes? % "hive-test") (str/split-lines out))]
    (is (= 2 (count lines)))
    (is (some #(re-find #"\bdeps\.edn\b" %) lines))
    (is (some #(re-find #"\bbb\.edn\b" %) lines))
    (is (apply distinct? lines))))

(deftest a-row-the-plan-holds-several-times-prints-once-with-its-count
  (let [row {:coord :mvn :project "hive-universe" :lib 'io.github.hive-agi/hive-events
             :old-version "0.5.14" :new-version "0.5.16" :source "clojars" :path "/w/hive-universe/deps.edn"}
        out (strip-ansi (with-out-str (print-changes! [row row row])))
        lines (filter #(str/includes? % "hive-events") (str/split-lines out))]
    (is (= 1 (count lines)))
    (is (str/includes? (first lines) "×3"))
    (is (str/includes? out "3 mismatches found") "the count still says what --apply will write")))