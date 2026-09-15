(ns bb-depsolve.core.upgrade-test
  "Tests for the upgrade command's file rewriting, project scoping and version
   selection. The rewrite fns are exercised on real temp files; `upgrade-cmd`
   runs either on paths that resolve zero libraries or over an injected memory
   resolver, so no registry is contacted."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.resolver :as resolver]
            [bb-depsolve.core.upgrade :as upgrade]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- workspace
  "Write each [project file content] triple under a fresh root. => root path"
  [files]
  (let [root (str (fs/create-temp-dir {:prefix "bb-depsolve-upgrade"}))]
    (doseq [[project fname content] files
            :let [f (fs/path root project fname)]]
      (fs/create-dirs (fs/parent f))
      (spit (str f) content))
    root))

(defn- strip-ansi [s] (str/replace s #"\[[0-9;]*m" ""))

;; =============================================================================
;; apply-mvn-change!
;; =============================================================================

(deftest apply-mvn-change!-rewrites-deps-edn-test
  (let [content "{:deps {acme/lib {:mvn/version \"1.0.0\"}\n        other/lib {:mvn/version \"1.0.0\"}}}"
        out (upgrade/apply-mvn-change! content {:type :deps-edn} 'acme/lib "1.1.0")]
    (is (str/includes? out "acme/lib {:mvn/version \"1.1.0\"}"))
    (is (str/includes? out "other/lib {:mvn/version \"1.0.0\"}")
        "only the named lib moves")))

(deftest apply-mvn-change!-rewrites-shadow-cljs-test
  (let [content "{:dependencies [[acme/lib \"1.0.0\"] [other/lib \"1.0.0\"]]}"
        out (upgrade/apply-mvn-change! content {:type :shadow-cljs-edn} 'acme/lib "1.1.0")]
    (is (str/includes? out "[acme/lib \"1.1.0\"]")
        "a shadow-cljs file keeps the Lein-style vector form")
    (is (str/includes? out "[other/lib \"1.0.0\"]"))))

;; =============================================================================
;; apply-mvn-upgrades!
;; =============================================================================

(deftest apply-mvn-upgrades!-writes-every-upgrade-per-file-test
  (let [root (workspace
              [["alpha" "deps.edn" "{:deps {a/one {:mvn/version \"1.0.0\"}\n        a/two {:mvn/version \"1.0.0\"}}}"]
               ["ui" "shadow-cljs.edn" "{:dependencies [[a/one \"1.0.0\"]]}"]])
        deps (str (fs/path root "alpha" "deps.edn"))
        shadow (str (fs/path root "ui" "shadow-cljs.edn"))
        index {deps {:path deps :type :deps-edn :project "alpha"}
               shadow {:path shadow :type :shadow-cljs-edn :project "ui"}}
        upgrades [{:path deps :lib 'a/one :new-version "2.0.0"}
                  {:path deps :lib 'a/two :new-version "3.0.0"}
                  {:path shadow :lib 'a/one :new-version "2.0.0"}]
        out (strip-ansi (with-out-str (upgrade/apply-mvn-upgrades! root upgrades index)))]
    (is (str/includes? (slurp deps) "a/one {:mvn/version \"2.0.0\"}"))
    (is (str/includes? (slurp deps) "a/two {:mvn/version \"3.0.0\"}")
        "two upgrades to one file both survive the single write")
    (is (str/includes? (slurp shadow) "[a/one \"2.0.0\"]"))
    (is (str/includes? out "Updated alpha/deps.edn"))
    (is (str/includes? out "Updated ui/shadow-cljs.edn"))
    (is (str/includes? out "Applied 3 upgrades across 2 files."))))

;; =============================================================================
;; upgrade-cmd
;; =============================================================================

(deftest upgrade-cmd-with-no-mvn-deps-is-up-to-date-test
  (let [root (workspace [["alpha" "deps.edn" "{:deps {}}"]])
        out (strip-ansi (with-out-str (upgrade/upgrade-cmd {:opts {:root root}})))]
    (is (str/includes? out "Checking 0 unique libraries"))
    (is (str/includes? out "Resolved 0 / 0 libraries"))
    (is (not (str/includes? out "NO registry could resolve")))
    (is (str/includes? out "All mvn deps are up to date."))))

(deftest upgrade-cmd-project-scopes-the-scan-test
  (testing "--project drops every other project's libraries before resolution"
    (let [root (workspace
                [["alpha" "deps.edn" "{:deps {}}"]
                 ["beta" "deps.edn" "{:deps {acme/lib {:mvn/version \"1.0.0\"}}}"]])
          out (strip-ansi
               (with-out-str (upgrade/upgrade-cmd {:opts {:root root :project "alpha"}})))]
      (is (str/includes? out "Checking 0 unique libraries")
          "beta's acme/lib would make this 1")
      (is (str/includes? out "All mvn deps are up to date.")))))

;; =============================================================================
;; upgrade-cmd over an injected resolver
;; =============================================================================

(def ^:private alpha
  ["alpha" "deps.edn"
   (str "{:deps {acme/lib {:mvn/version \"1.0.0\"}\n"
        "        other/lib {:mvn/version \"2.0.0\"}\n"
        "        gone/lib {:mvn/version \"1.0.0\"}}}")])

(def ^:private latest
  {'acme/lib  [{:id "clojars" :public? true :version "1.2.0"}
               {:id "gitea" :public? false :version "1.1.0"}]
   'other/lib [{:id "clojars" :public? true :version "2.0.0"}]})

(defn- run-with [res opts]
  (strip-ansi (with-out-str (upgrade/upgrade-cmd res {:opts opts}))))

(deftest upgrade-cmd-reports-the-highest-version-across-registries-test
  (let [root (workspace [alpha])
        deps (str (fs/path root "alpha" "deps.edn"))
        before (slurp deps)
        out (run-with (resolver/memory-resolver {:latest latest}) {:root root})]
    (is (str/includes? out "Checking 3 unique libraries"))
    (is (str/includes? out "Resolved 2 / 3 libraries"))
    (is (str/includes? out "1 upgrades available across 1 libraries:"))
    (is (re-find #"acme/lib\s+1\.0\.0 -> 1\.2\.0\s+\(alpha\)" out)
        "the candidate is the max over every registry's latest")
    (is (not (re-find #"other/lib\s+2\.0\.0 ->" out))
        "a lib already at its latest is not an upgrade")
    (is (str/includes? out "Dry run."))
    (is (= before (slurp deps)) "a dry run writes nothing")))

(deftest upgrade-cmd-names-libraries-no-registry-resolved-test
  (let [out (run-with (resolver/memory-resolver {:latest latest})
                      {:root (workspace [alpha])})]
    (is (str/includes? out "1 library(ies) NO registry could resolve"))
    (is (re-find #"(?m)^\s+gone/lib$" out))))

(deftest upgrade-cmd-threads-pre-release-to-the-resolver-test
  (let [res (resolver/memory-resolver {:latest latest})]
    (run-with res {:root (workspace [alpha]) :pre-release true})
    (is (= #{true} (set (map last @(:calls res)))))
    (is (= #{'acme/lib 'other/lib 'gone/lib} (set (map second @(:calls res))))
        "each unique lib is resolved")))

(deftest upgrade-cmd-apply-writes-the-selected-upgrades-test
  (testing "with no TTY every upgrade is applied"
    (let [root (workspace [alpha])
          deps (str (fs/path root "alpha" "deps.edn"))
          out (run-with (resolver/memory-resolver {:latest latest})
                        {:root root :apply true})]
      (is (str/includes? out "No TTY"))
      (is (str/includes? (slurp deps) "acme/lib {:mvn/version \"1.2.0\"}"))
      (is (str/includes? (slurp deps) "other/lib {:mvn/version \"2.0.0\"}"))
      (is (str/includes? (slurp deps) "gone/lib {:mvn/version \"1.0.0\"}")
          "an unresolved lib is left alone"))))

(deftest resolve-latest-selection-is-injectable-test
  (let [res (resolver/memory-resolver
             {:latest (assoc latest 'priv/lib [{:id "clojars" :public? true :version "0.1.0"}
                                               {:id "gitea" :public? false :version "0.1.1"}])})
        public-only (fn [rows] (resolver/latest-of (filter :public? rows)))]
    (is (= {:ok "1.2.0"} (resolver/resolve-latest res 'acme/lib false)))
    (is (= {:ok "0.1.1"} (resolver/resolve-latest res 'priv/lib false))
        "the default selection is the max over every registry")
    (is (= {:ok "0.1.0"} (resolver/resolve-latest res 'priv/lib false public-only))
        "the rows keep provenance, so a consumer-aware selection can drop a private-only version")
    (is (= :io/no-published-version
           (:error (resolver/resolve-latest res 'gone/lib false))))))
