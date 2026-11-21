(ns bb-depsolve.cascade-workspace-test
  "Cascade and sync over a real fixture workspace.

   The workspace holds what ~/PP/hive holds and the planner got wrong:
     lib-a              canonical checkout; VERSION 1.0.0, tags run to v1.0.2,
                        one commit past it (the only real seed)
     lib-a-wt-feature   linked git worktree of lib-a with a branch commit of
                        its own: a branch, not a library
     vendored           listed in depsolve-skip.edn, with unpublished commits
     app-runtime        pins lib-a in :deps; VERSION 0.1.0 but HEAD is v0.1.1
                        (a stale VERSION that must neither seed nor be planned from)
     app-alias          pins lib-a only under an alias

   Registry lookups go through an injected IArtifactRegistry; nothing here
   touches the network."
  (:require [babashka.fs :as fs]
            [babashka.process :as proc]
            [bb-depsolve.cascade.plan :as cas]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.skip :as skip]
            [bb-depsolve.release.port :as port]
            [bb-depsolve.release.run :as run]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- git! [dir & args]
  (let [r (proc/sh (into ["git" "-C" (str dir)] args))]
    (when-not (zero? (:exit r))
      (throw (ex-info "git failed" {:args args :err (:err r)})))
    r))

(defn- write! [dir rel content]
  (let [f (fs/path dir rel)]
    (fs/create-dirs (fs/parent f))
    (spit (str f) content)))

(defn- commit! [dir msg]
  (git! dir "add" "-A")
  (git! dir "commit" "-q" "-m" msg))

(defn- repo!
  "A git repo at ROOT/NAME publishing io.github.test/NAME, VERSION VERSION,
   DEPS-EDN as deps.edn, committed once."
  [root name version deps-edn]
  (let [dir (fs/path root name)]
    (fs/create-dirs dir)
    (git! dir "init" "-q" "-b" "main")
    (git! dir "config" "user.name" "fixture")
    (git! dir "config" "user.email" "fixture@example.invalid")
    (git! dir "config" "commit.gpgsign" "false")
    (git! dir "config" "tag.gpgsign" "false")
    (write! dir "version.edn" (pr-str {:lib (symbol "io.github.test" name)}))
    (write! dir "VERSION" (str version "\n"))
    (write! dir "deps.edn" deps-edn)
    (commit! dir "init")
    (str dir)))

(defn workspace!
  "The fixture workspace. => canonical root path."
  []
  (let [root (str (fs/canonicalize (fs/create-temp-dir {:prefix "depsolve-cascade-ws"})))
        lib-a (repo! root "lib-a" "1.0.0" "{:deps {}}")]
    (git! lib-a "tag" "v1.0.0")
    (write! lib-a "src/a.clj" "(ns a)")
    (commit! lib-a "feat: a")
    (git! lib-a "tag" "v1.0.2")
    (write! lib-a "src/a.clj" "(ns a) (def x 1)")
    (commit! lib-a "feat: unpublished")

    (git! lib-a "worktree" "add" "-q" (str (fs/path root "lib-a-wt-feature")) "-b" "feature")
    (let [wt (str (fs/path root "lib-a-wt-feature"))]
      (write! wt "src/a.clj" "(ns a) (def x 2)")
      (commit! wt "feat: branch work"))

    (let [vendored (repo! root "vendored" "0.9.0" "{:deps {io.github.test/lib-a {:mvn/version \"1.0.2\"}}}")]
      (git! vendored "tag" "v0.9.0")
      (write! vendored "src/v.clj" "(ns v)")
      (commit! vendored "local hack"))
    (write! root skip/default-file-name
            (pr-str {:skip [{:dir "vendored" :reason "not ours to publish"}]}))

    (let [app (repo! root "app-runtime" "0.1.0"
                     "{:deps {io.github.test/lib-a {:mvn/version \"1.0.2\"}}}")]
      (git! app "tag" "v0.1.0")
      (write! app "src/app.clj" "(ns app)")
      (commit! app "feat: released as 0.1.1 by CI")
      (git! app "tag" "v0.1.1"))

    (let [alias-app (repo! root "app-alias" "0.2.0"
                           "{:deps {} :aliases {:test {:extra-deps {io.github.test/lib-a {:mvn/version \"1.0.2\"}}}}}")]
      (git! alias-app "tag" "v0.2.0"))
    root))

(defn- steps [plan]
  (into {} (for [w (:waves plan) s (:steps w)] [(:project s) (assoc s :wave (:index w))])))

(defn- plan-of
  ([root] (plan-of root {}))
  ([root published]
   (run/cascade-plan {:root root :org "test"}
                     (port/memory-port {:registry published}))))

;; =============================================================================
;; DEPSOLVE-WT
;; =============================================================================

(deftest a-linked-worktree-is-not-a-library-test
  (let [root (workspace!)
        plan (plan-of root)
        planned (set (cas/plan-projects plan))]
    (is (not (contains? planned "lib-a-wt-feature"))
        "the worktree is a branch of lib-a, so it is never released")
    (is (not (contains? (:seeds plan) "lib-a-wt-feature")))
    (is (not-any? #(str/includes? (:path %) "lib-a-wt-feature")
                  (mapcat :pin-updates (mapcat :steps (:waves plan))))
        "no pin row is planned inside the worktree")
    (testing "lib-a resolves to its canonical checkout"
      (is (= (str (fs/path root "lib-a")) (:dir (get (steps plan) "lib-a"))))
      (is (= ["lib-a"] (mapv :dep (:pin-updates (get (steps plan) "app-runtime"))))
          "a consumer re-pins the library, never the worktree's dir name"))))

(deftest the-skip-file-bounds-the-cascade-test
  (let [plan (plan-of (workspace!))]
    (is (not (contains? (:seeds plan) "vendored")))
    (is (not (contains? (set (cas/plan-projects plan)) "vendored")))))

(deftest sync-scans-neither-a-worktree-nor-a-skipped-dir-test
  (let [root (workspace!)
        projects (set (map :project (discovery/find-dep-files {:root root})))]
    (is (= #{"lib-a" "app-runtime" "app-alias"} projects))
    (testing "--root pointing AT a worktree still scans it"
      (is (= #{"lib-a-wt-feature"}
             (set (map :project (discovery/find-dep-files {:root (str (fs/path root "lib-a-wt-feature"))}))))))))

(deftest a-worktree-whose-checkout-lives-elsewhere-is-kept-test
  (testing "a sweep directory holding only worktrees still sees them"
    (let [root (workspace!)
          sweep (str (fs/canonicalize (fs/create-temp-dir {:prefix "depsolve-sweep"})))]
      (git! (fs/path root "lib-a") "worktree" "add" "-q" (str (fs/path sweep "lib-a")) "-b" "sweep")
      (is (= #{"lib-a"} (set (map :project (discovery/find-dep-files {:root sweep}))))))))

;; =============================================================================
;; DEPSOLVE-VERSION
;; =============================================================================

(deftest seeds-come-from-the-newest-release-not-from-VERSION-test
  (let [plan (plan-of (workspace!))]
    (is (= #{"lib-a"} (set (:seeds plan)))
        "app-runtime's VERSION lags, but HEAD is its v0.1.1: nothing unpublished")))

(deftest the-newest-tag-is-the-version-floor-test
  (let [s (steps (plan-of (workspace!)))]
    (is (= "1.0.2" (get-in s ["lib-a" :current-version])))
    (is (= "1.0.3" (get-in s ["lib-a" :next-version])))
    (is (= "0.1.1" (get-in s ["app-runtime" :current-version])))
    (is (= "0.1.2" (get-in s ["app-runtime" :next-version]))
        "planning from VERSION would cut 0.1.1 again")))

(deftest the-newest-published-artifact-is-the-version-floor-test
  (let [s (steps (plan-of (workspace!) {'io.github.test/lib-a #{"1.0.4"}}))]
    (is (= "1.0.4" (get-in s ["lib-a" :current-version])))
    (is (= "1.0.5" (get-in s ["lib-a" :next-version])))
    (is (= {:declared "1.0.0" :observed "1.0.4"} (get-in s ["lib-a" :version-drift])))))

(deftest an-alias-only-consumer-gets-a-wave-test
  (let [s (steps (plan-of (workspace!)))
        alias-step (get s "app-alias")]
    (is (some? alias-step) "the alias pin must be re-pinned after lib-a releases")
    (is (> (:wave alias-step) (get-in s ["lib-a" :wave])))
    (is (= [{:dep "lib-a" :from "1.0.2" :to "1.0.3"}]
           (mapv #(select-keys % [:dep :from :to]) (:pin-updates alias-step))))))
