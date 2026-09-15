(ns bb-depsolve.core.upgrade-test
  "Tests for the upgrade command's file rewriting, project scoping and version
   selection. The rewrite fns are exercised on real temp files; `upgrade-cmd`
   runs either on paths that resolve zero libraries or over an injected memory
   resolver, so no registry is contacted."
  (:require [babashka.fs :as fs]
            [bb-depsolve.cli.ui :as ui]
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

(deftest upgrade-cmd-apply-writes-the-picked-upgrades-test
  (testing "an interactive pick applies exactly the chosen libs"
    (let [root (workspace [alpha])
          deps (str (fs/path root "alpha" "deps.edn"))
          out (with-redefs [ui/gum-filter (fn [choices _] (vec choices))]
                (run-with (resolver/memory-resolver {:latest latest})
                          {:root root :apply true}))]
      (is (str/includes? out "Applied 1 upgrades across 1 files."))
      (is (str/includes? (slurp deps) "acme/lib {:mvn/version \"1.2.0\"}"))
      (is (str/includes? (slurp deps) "other/lib {:mvn/version \"2.0.0\"}"))
      (is (str/includes? (slurp deps) "gone/lib {:mvn/version \"1.0.0\"}")
          "an unresolved lib is left alone"))))

(deftest upgrade-cmd-apply-selecting-nothing-writes-nothing-test
  (let [root (workspace [alpha])
        deps (str (fs/path root "alpha" "deps.edn"))
        before (slurp deps)
        out (with-redefs [ui/gum-filter (fn [_ _] [])]
              (run-with (resolver/memory-resolver {:latest latest})
                        {:root root :apply true}))]
    (is (str/includes? out "No upgrades selected."))
    (is (= before (slurp deps)))))

;; =============================================================================
;; The guard: no-TTY refusal, lib filters, held jumps, per-consumer registries
;; =============================================================================

(defn- no-tty
  "Run F with `ui/gum-filter` answering the way it does with no TTY, recording
   every exit code the command asks for. => [printed-output exit-codes]"
  [f]
  (let [exits (atom [])
        out (with-redefs [ui/gum-filter (fn [_ _] nil)]
              (binding [upgrade/*exit!* (fn [code] (swap! exits conj code))]
                (strip-ansi (with-out-str (f)))))]
    [out @exits]))

(defn- run-no-tty
  [res opts]
  (no-tty #(upgrade/upgrade-cmd res {:opts opts})))

(deftest upgrade-cmd-apply-refuses-without-a-tty-or-a-selection-test
  (let [root (workspace [alpha])
        deps (str (fs/path root "alpha" "deps.edn"))
        before (slurp deps)
        [out exits] (run-no-tty (resolver/memory-resolver {:latest latest})
                                {:root root :apply true})]
    (is (str/includes? out "Refusing --apply"))
    (is (str/includes? out "--all") "the refusal names the flags that release it")
    (is (str/includes? out "--only"))
    (is (= [1] exits) "the refusal exits non-zero")
    (is (= before (slurp deps)) "a refused apply writes nothing")))

(deftest upgrade-cmd-all-applies-every-listed-upgrade-without-a-tty-test
  (let [root (workspace [alpha])
        deps (str (fs/path root "alpha" "deps.edn"))
        [out exits] (run-no-tty (resolver/memory-resolver {:latest latest})
                                {:root root :apply true :all true})]
    (is (= [] exits))
    (is (str/includes? out "Applied 1 upgrades across 1 files."))
    (is (str/includes? (slurp deps) "acme/lib {:mvn/version \"1.2.0\"}"))))

(def ^:private two-upgrades
  {'acme/lib  [{:id "clojars" :public? true :version "1.2.0"}]
   'other/lib [{:id "clojars" :public? true :version "2.1.0"}]})

(deftest upgrade-cmd-only-names-the-selection-without-a-tty-test
  (let [root (workspace [alpha])
        deps (str (fs/path root "alpha" "deps.edn"))
        [out exits] (run-no-tty (resolver/memory-resolver {:latest two-upgrades})
                                {:root root :apply true :only "acme/lib"})]
    (is (= [] exits) "an explicit selection needs no TTY")
    (is (str/includes? out "dropped by --only / --exclude"))
    (is (str/includes? (slurp deps) "acme/lib {:mvn/version \"1.2.0\"}"))
    (is (str/includes? (slurp deps) "other/lib {:mvn/version \"2.0.0\"}")
        "a lib --only did not name is left alone")))

(deftest upgrade-cmd-exclude-drops-a-lib-test
  (let [root (workspace [alpha])
        deps (str (fs/path root "alpha" "deps.edn"))
        [out _] (run-no-tty (resolver/memory-resolver {:latest two-upgrades})
                            {:root root :apply true :all true :exclude "acme/lib"})]
    (is (not (str/includes? out "acme/lib")))
    (is (str/includes? (slurp deps) "acme/lib {:mvn/version \"1.0.0\"}")
        "an excluded lib is never resolved nor written")
    (is (str/includes? (slurp deps) "other/lib {:mvn/version \"2.1.0\"}"))))

(def ^:private jumps
  ["big" "deps.edn" (str "{:deps {tika/core {:mvn/version \"3.3.2\"}\n"
                         "        raster/raster {:mvn/version \"0.2.318\"}}}")])

(def ^:private jump-latest
  {'tika/core     [{:id "clojars" :public? true :version "4.0.0"}]
   'raster/raster [{:id "clojars" :public? true :version "0.4.1"}]})

(deftest upgrade-cmd-holds-majors-and-pre-1-0-minor-jumps-test
  (let [root (workspace [jumps])
        deps (str (fs/path root "big" "deps.edn"))
        before (slurp deps)
        [out _] (run-no-tty (resolver/memory-resolver {:latest jump-latest})
                            {:root root :apply true :all true})]
    (is (str/includes? out "2 upgrade(s) HELD"))
    (is (re-find #"tika/core\s+3\.3\.2 -> 4\.0\.0\s+major" out) "held ones are listed")
    (is (re-find #"raster/raster\s+0\.2\.318 -> 0\.4\.1\s+major" out))
    (is (str/includes? out "every candidate was held"))
    (is (= before (slurp deps)) "a held jump is never written"))
  (testing "--allow-major releases them"
    (let [root (workspace [jumps])
          deps (str (fs/path root "big" "deps.edn"))
          [out _] (run-no-tty (resolver/memory-resolver {:latest jump-latest})
                              {:root root :apply true :all true :allow-major true})]
      (is (not (str/includes? out "HELD")))
      (is (str/includes? (slurp deps) "tika/core {:mvn/version \"4.0.0\"}"))
      (is (str/includes? (slurp deps) "raster/raster {:mvn/version \"0.4.1\"}")))))

(def ^:private internal-and-external
  ["core" "deps.edn" (str "{:deps {io.github.hive-agi/hive-dsl {:mvn/version \"0.5.24\"}\n"
                          "        acme/lib {:mvn/version \"1.0.0\"}}}")])

(deftest upgrade-cmd-leaves-the-internal-org-to-sync-test
  (let [root (workspace [internal-and-external])
        deps (str (fs/path root "core" "deps.edn"))
        res (resolver/memory-resolver
             {:latest (assoc two-upgrades
                             'io.github.hive-agi/hive-dsl
                             [{:id "clojars" :public? true :version "0.5.27"}])})
        [out _] (run-no-tty res {:root root :apply true :all true})]
    (is (str/includes? out "1 internal library(ies) left to `sync`"))
    (is (= #{'acme/lib} (set (map second @(:calls res))))
        "an internal lib is not even resolved")
    (is (str/includes? (slurp deps) "io.github.hive-agi/hive-dsl {:mvn/version \"0.5.24\"}"))
    (is (str/includes? (slurp deps) "acme/lib {:mvn/version \"1.2.0\"}"))))

(def ^:private gitea-url "https://git.example.com/api/packages/acme/maven")

(def ^:private split-latest
  {'acme/lib [{:id "clojars" :url "https://repo.clojars.org/" :public? true :version "1.1.0"}
              {:id "gitea" :url gitea-url :public? false :version "1.2.0"}]})

(deftest upgrade-cmd-pins-each-consumer-to-its-own-registries-test
  (let [root (workspace
              [["pub" "deps.edn" "{:deps {acme/lib {:mvn/version \"1.0.0\"}}}"]
               ["priv" "deps.edn" (str "{:mvn/repos {\"gitea\" {:url \"" gitea-url "\"}}\n"
                                       " :deps {acme/lib {:mvn/version \"1.0.0\"}}}")]])
        pub (str (fs/path root "pub" "deps.edn"))
        priv (str (fs/path root "priv" "deps.edn"))
        [_ exits] (run-no-tty (resolver/memory-resolver {:latest split-latest})
                              {:root root :apply true :all true})]
    (is (= [] exits))
    (is (str/includes? (slurp pub) "acme/lib {:mvn/version \"1.1.0\"}")
        "a public-only project stops at the version Clojars serves")
    (is (str/includes? (slurp priv) "acme/lib {:mvn/version \"1.2.0\"}")
        "the project declaring the private registry gets its version")))

(deftest upgrade-cmd-names-a-version-the-consumer-cannot-fetch-test
  (let [root (workspace [["pub" "deps.edn" "{:deps {acme/lib {:mvn/version \"1.1.0\"}}}"]])
        deps (str (fs/path root "pub" "deps.edn"))
        before (slurp deps)
        [out _] (run-no-tty (resolver/memory-resolver {:latest split-latest})
                            {:root root :apply true :all true})]
    (is (re-find #"acme/lib\s+1\.1\.0 -> 1\.2\.0\s+only on registry gitea" out))
    (is (= before (slurp deps)))))

(deftest upgrade-cmd-refuses-a-downgrade-test
  (let [root (workspace [["pub" "deps.edn" "{:deps {acme/lib {:mvn/version \"1.2.0\"}}}"]])
        deps (str (fs/path root "pub" "deps.edn"))
        before (slurp deps)
        [out _] (run-no-tty (resolver/memory-resolver {:latest split-latest})
                            {:root root :apply true :all true :allow-major true})]
    (is (re-find #"acme/lib\s+1\.2\.0 -> 1\.1\.0\s+refused: moves the pin DOWN" out)
        "the newest version the consumer can reach is older than its pin")
    (is (= before (slurp deps)) "--all never writes a downgrade")))

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
