(ns bb-depsolve.var-seam-test
  "Invariant gate for the bb-depsolve re-export facades (epic
   20260821211613-0c0e618f): core.api, version.api and core.registry.

   A `(def x owner/x)` re-export stores the fn OBJECT, so a fix reloaded into
   the owner (a bb nREPL, a JVM REPL) never reaches callers entering through
   the facade. Each fn re-export is now a delegating defn; data re-exports
   (default-skip-dirs, default-depth, public-repo-hosts) stay defs. This ns
   rebinds a leaf and asserts it is seen through the facade, keeps a control
   showing a captured copy is stale, and fails if a fn-valued frozen alias
   returns to the facade files."
  (:require [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.core.api :as core-api]
            [bb-depsolve.core.registry :as registry]
            [bb-depsolve.version.api :as vapi]
            [bb-depsolve.version.repos :as repos]
            [bb-depsolve.version.semver :as semver]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(defn- with-rebound-leaf
  "alter-var-root, not with-redefs: the rebinding must reach code that
   resolves the var LATER, which is what a reload does."
  [leaf-var replacement f]
  (let [original @leaf-var]
    (try
      (alter-var-root leaf-var (constantly replacement))
      (f)
      (finally (alter-var-root leaf-var (constantly original))))))

(deftest facade-sees-a-rebound-leaf-test
  (testing "version.semver/bump-patch rebound is seen through version.api"
    (with-rebound-leaf #'semver/bump-patch (fn [& _] ::rebound)
      (fn [] (is (= ::rebound (vapi/bump-patch [1 2 3]))))))
  (testing "version.repos/public-repo? rebound is seen through core.registry"
    (with-rebound-leaf #'repos/public-repo? (fn [& _] ::rebound)
      (fn [] (is (= ::rebound (registry/public-repo? {:url "x"}))))))
  (testing "core.api delegates through its owner var too"
    (with-rebound-leaf #'ui/c (fn [& _] ::rebound)
      (fn [] (is (= ::rebound (core-api/c :red "x")))))))

(deftest frozen-copy-is-stale-control-test
  (let [frozen semver/bump-patch]
    (with-rebound-leaf #'semver/bump-patch (fn [& _] ::rebound)
      (fn []
        (is (= ::rebound (vapi/bump-patch nil)) "the facade followed the var")
        (is (not (identical? frozen @#'semver/bump-patch))
            "the captured object still runs the OLD fn: the staleness removed")))))

(deftest data-reexports-stay-values-test
  (is (identical? repos/public-repo-hosts registry/public-repo-hosts))
  (is (not (fn? core-api/default-skip-dirs)))
  (is (not (fn? core-api/default-depth))))

(def ^:private facade-files
  {'bb-depsolve.core.api      "src/bb_depsolve/core/api.clj"
   'bb-depsolve.version.api   "src/bb_depsolve/version/api.clj"
   'bb-depsolve.core.registry "src/bb_depsolve/core/registry.clj"})

(def ^:private frozen-alias-pattern
  #"(?m)^\(def\s+([^\s\)\^]+)\s+([a-zA-Z][\w.\-]*/[^\s\)]+)\s*\)")

(deftest no-fn-valued-frozen-alias-test
  (let [offenders (for [[ns-sym path] facade-files
                        :let [f (io/file path)]
                        :when (.exists f)
                        [_ n _] (re-seq frozen-alias-pattern (slurp f))
                        :let [v (ns-resolve ns-sym (symbol n))]
                        :when (and v (fn? @v))]
                    (str ns-sym "/" n))]
    (is (every? #(.exists (io/file %)) (vals facade-files))
        "every facade file was found (non-vacuity)")
    (is (= [] (vec (sort offenders)))
        "these defs capture a fn object instead of delegating through the var")))
