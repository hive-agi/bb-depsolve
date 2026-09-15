(ns bb-depsolve.core.upgrade-test
  "Tests for the upgrade command's file rewriting and project scoping. The
   rewrite fns are exercised on real temp files; `upgrade-cmd` only on paths
   that resolve zero libraries, so no registry is contacted."
  (:require [babashka.fs :as fs]
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
