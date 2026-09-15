(ns bb-depsolve.core.tree-test
  "Tests for the tree command over a throwaway workspace. Every run passes
   `:tree-depth 0`, the documented bound at which no child is resolved, so the
   command never reaches a registry."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.tree :as tree]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- workspace
  "Write each [project file content] triple under a fresh root. => root path"
  [files]
  (let [root (str (fs/create-temp-dir {:prefix "bb-depsolve-tree"}))]
    (doseq [[project fname content] files
            :let [f (fs/path root project fname)]]
      (fs/create-dirs (fs/parent f))
      (spit (str f) content))
    root))

(defn- strip-ansi [s] (str/replace s #"\[[0-9;]*m" ""))

(defn- run [root & {:as opts}]
  (strip-ansi
   (with-out-str
     (tree/tree-cmd {:opts (merge {:root root :tree-depth 0} opts)}))))

(def ^:private clean
  ["clean" "deps.edn" "{:deps {org.clojure/clojure {:mvn/version \"1.12.0\"}}}"])

(def ^:private conflicted
  "The same lib pinned at two versions in one dep file: once in :deps, once in
   an alias."
  ["conflicted" "deps.edn"
   (str "{:deps {acme/lib {:mvn/version \"1.0.0\"}}\n"
        " :aliases {:dev {:extra-deps {acme/lib {:mvn/version \"2.0.0\"}}}}}")])

(deftest tree-prints-each-project-and-its-direct-deps-test
  (let [out (run (workspace [clean]))]
    (is (str/includes? out "Building dependency tree..."))
    (is (re-find #"(?m)^clean\s+\(clean/deps\.edn\)$" out)
        "the project is labelled with its dep file relative to the root")
    (is (str/includes? out "org.clojure/clojure"))
    (is (not (str/includes? out "conflict(s)")))))

(deftest tree-reports-conflicts-test
  (let [out (run (workspace [conflicted]))]
    (is (str/includes? out "1 conflict(s):"))
    (is (re-find #"acme/lib \S+ 1\.0\.0 vs 2\.0\.0" out)
        "versions are listed in ascending order")))

(deftest tree-conflicts-only-hides-clean-projects-test
  (let [out (run (workspace [clean conflicted]) :conflicts-only true)]
    (is (not (re-find #"(?m)^clean\s+\(" out)))
    (is (re-find #"(?m)^conflicted\s+\(conflicted/deps\.edn\)$" out))
    (is (str/includes? out "1 conflict(s):"))
    (testing "the tree body is suppressed, only the conflict summary remains"
      (is (not (str/includes? out "org.clojure/clojure"))))))

(deftest tree-resolved-prints-the-nearest-wins-choice-test
  (let [out (run (workspace [clean]) :resolved true)]
    (is (str/includes? out "Resolved (1 libs, nearest-wins):"))
    (is (re-find #"org\.clojure/clojure 1\.12\.0\s+\(depth 0\)" out))))
