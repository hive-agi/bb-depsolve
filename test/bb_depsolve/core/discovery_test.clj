(ns bb-depsolve.core.discovery-test
  "Tests for workspace scanning: dep-file discovery and project layout."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.discovery :as disc]
            [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- touch! [dir & segments]
  (let [f (apply fs/path dir segments)]
    (fs/create-dirs (fs/parent f))
    (spit (fs/file f) "")
    f))

(defn- workspace
  "Build a throwaway workspace:
     alpha/      git + VERSION + deps.edn + bb.edn   -> a project
     beta/       git + VERSION + deps.edn            -> a project
     gamma/      git + deps.edn                      -> no VERSION, not a project
     delta/      VERSION + deps.edn                   -> no .git, not a project
     ui/         shadow-cljs.edn                      -> dep file, not a project
     vendor/     git + VERSION + deps.edn             -> skipped by default
     deps.edn                                         -> root-level dep file"
  []
  (let [root (str (fs/create-temp-dir {:prefix "bb-depsolve-disc"}))]
    (doseq [[dir files] {"alpha"  [".git/HEAD" "VERSION" "deps.edn" "bb.edn"]
                         "beta"   [".git/HEAD" "VERSION" "deps.edn"]
                         "gamma"  [".git/HEAD" "deps.edn"]
                         "delta"  ["VERSION" "deps.edn"]
                         "ui"     ["shadow-cljs.edn"]
                         "vendor" [".git/HEAD" "VERSION" "deps.edn"]}
            f files]
      (touch! root dir f))
    (touch! root "deps.edn")
    root))

(defn- names [paths] (mapv (comp str fs/file-name) paths))

;; =============================================================================
;; Unit — skip-path?
;; =============================================================================

(deftest skip-path?-matches-a-skip-dir-and-its-subtree-test
  (let [root "/w"]
    (is (disc/skip-path? root #{"vendor"} "/w/vendor"))
    (is (disc/skip-path? root #{"vendor"} "/w/vendor/lib/deps.edn")
        "a skipped directory takes its whole subtree with it")
    (is (not (disc/skip-path? root #{"vendor"} "/w/vendored")))
    (is (not (disc/skip-path? root #{"vendor"} "/w/src/vendor"))
        "skip-dirs are matched relative to the root, not anywhere in the path")))

;; =============================================================================
;; Unit — find-workspace-projects
;; =============================================================================

(deftest find-workspace-projects-needs-both-git-and-version-test
  (let [root (workspace)]
    (is (= ["alpha" "beta"]
           (names (disc/find-workspace-projects root disc/default-skip-dirs)))
        "gamma has no VERSION, delta has no .git, vendor is skipped")))

(deftest find-workspace-projects-honours-explicit-skip-dirs-test
  (let [root (workspace)]
    (is (= ["beta" "vendor"] (names (disc/find-workspace-projects root #{"alpha"})))
        "an explicit skip set replaces the default — vendor is not skipped intrinsically")
    (is (= ["alpha" "beta" "vendor"]
           (names (disc/find-workspace-projects root #{}))))))

;; =============================================================================
;; Unit — find-dep-files
;; =============================================================================

(deftest find-dep-files-scans-one-level-by-default-test
  (let [root (workspace)
        found (disc/find-dep-files {:root root})]
    (is (= #{["alpha" :deps-edn] ["alpha" :bb-edn]
             ["beta" :deps-edn] ["gamma" :deps-edn] ["delta" :deps-edn]
             ["ui" :shadow-cljs-edn]
             [(str (fs/file-name root)) :deps-edn]}
           (set (map (juxt :project :type) found)))
        "every dep file one level down, vendor excluded, root file included")
    (is (every? #(fs/exists? (:path %)) found))))

(deftest find-dep-files-at-depth-zero-reads-the-root-itself-test
  (let [root (workspace)
        found (disc/find-dep-files {:root root :depth 0})]
    (is (= [:deps-edn] (mapv :type found)))
    (is (= [(str (fs/file-name root))] (mapv :project found)))))

(deftest find-dep-files-honours-explicit-skip-dirs-test
  (let [root (workspace)
        projects (set (map :project (disc/find-dep-files {:root root
                                                          :skip-dirs #{"alpha" "ui"}})))]
    (is (= #{"beta" "gamma" "delta" "vendor" (str (fs/file-name root))} projects)
        "an explicit skip set replaces the default — vendor comes back; root always included")))

;; =============================================================================
;; Unit — file-type dispatch
;; =============================================================================

(deftest shadow-deps-file?-test
  (is (disc/shadow-deps-file? {:type :shadow-cljs-edn}))
  (is (not (disc/shadow-deps-file? {:type :deps-edn})))
  (is (not (disc/shadow-deps-file? {:type :bb-edn}))))

(deftest extract-mvn-deps-dispatches-on-file-type-test
  (testing "deps.edn and bb.edn read :mvn/version coords"
    (is (= [{:lib 'acme/lib :version "1.2.3"}]
           (mapv #(select-keys % [:lib :version])
                 (disc/extract-mvn-deps {:type :deps-edn}
                                        "{:deps {acme/lib {:mvn/version \"1.2.3\"}}}")))))
  (testing "shadow-cljs.edn reads Lein-style vectors"
    (is (= [{:lib 'acme/lib :version "1.2.3"}]
           (mapv #(select-keys % [:lib :version])
                 (disc/extract-mvn-deps {:type :shadow-cljs-edn}
                                        "{:dependencies [[acme/lib \"1.2.3\"]]}")))))
  (testing "the shadow reader is not applied to a deps.edn"
    (is (= [] (disc/extract-mvn-deps {:type :deps-edn}
                                     "{:dependencies [[acme/lib \"1.2.3\"]]}")))))
