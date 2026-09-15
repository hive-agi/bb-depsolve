(ns bb-depsolve.core.tree-test
  "Tests for the tree command over a throwaway workspace. Runs on the default
   (live) resolver pass `:tree-depth 0`, the documented bound at which no child
   is resolved; runs that resolve children inject a memory resolver, so the
   command never reaches a registry."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.resolver :as resolver]
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

(defn- run-with [resolver root & {:as opts}]
  (strip-ansi
   (with-out-str
     (tree/tree-cmd resolver {:opts (merge {:root root} opts)}))))

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

;; =============================================================================
;; Resolved children (injected resolver)
;; =============================================================================

(def ^:private diamond
  "app pins acme/a 1.0.0 and acme/b 1.0.0; acme/a depends on acme/b 2.0.0."
  ["app" "deps.edn"
   "{:deps {acme/a {:mvn/version \"1.0.0\"} acme/b {:mvn/version \"1.0.0\"}}}"])

(def ^:private diamond-children
  {['acme/a "1.0.0"] [{:lib 'acme/b :version "2.0.0" :type :mvn}]
   ['acme/b "1.0.0"] []
   ['acme/b "2.0.0"] []})

(deftest tree-prints-resolved-children-indented-test
  (let [res (resolver/memory-resolver {:children diamond-children})
        out (run-with res (workspace [diamond]) :tree-depth 2)]
    (is (re-find #"(?m)^acme/a 1\.0\.0$" out))
    (is (re-find #"(?m)^  acme/b 2\.0\.0$" out)
        "a child is printed one level under its parent")
    (is (some #{[:dep-children 'acme/a "1.0.0"]} @(:calls res))
        "children are read from the injected resolver")))

(deftest tree-flags-a-conflict-introduced-by-a-transitive-dep-test
  (let [out (run-with (resolver/memory-resolver {:children diamond-children})
                      (workspace [diamond]) :tree-depth 2 :resolved true)]
    (is (str/includes? out "1 conflict(s):"))
    (is (re-find #"acme/b \S+ 1\.0\.0 vs 2\.0\.0" out))
    (testing "nearest-wins keeps the direct pin over the deeper one"
      (is (re-find #"acme/b 1\.0\.0\s+\(depth 0\)" out)))))

(deftest tree-depth-bounds-how-far-children-are-resolved-test
  (let [res (resolver/memory-resolver {:children diamond-children})
        out (run-with res (workspace [diamond]) :tree-depth 1)]
    (is (re-find #"(?m)^  acme/b 2\.0\.0$" out))
    (is (not (some #{[:dep-children 'acme/b "2.0.0"]} @(:calls res)))
        "a node at the depth bound is not asked for its children")))

(deftest tree-treats-an-unresolvable-dep-as-a-leaf-test
  (let [res (resolver/memory-resolver {})
        out (run-with res (workspace [diamond]) :tree-depth 2)]
    (is (re-find #"(?m)^acme/a 1\.0\.0$" out))
    (is (not (str/includes? out "conflict(s)"))
        "a resolver error yields no children, not a failure")
    (is (= #{[:dep-children 'acme/a "1.0.0"] [:dep-children 'acme/b "1.0.0"]}
           (set @(:calls res))))))
