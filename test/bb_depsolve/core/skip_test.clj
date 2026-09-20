(ns bb-depsolve.core.skip-test
  "The workspace skip file: what it excludes, and that every scan honours it."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.skip :as skip]
            [clojure.test :refer [deftest is testing]]))

(deftest an-entry-naming-no-directory-is-dropped-test
  (is (= #{"clojure-lsp"}
         (:dirs (skip/parse {:skip [{:reason "forgot the dir"}
                                    {:dir "clojure-lsp"}]})))))

(deftest a-bare-string-is-a-directory-with-no-reason-test
  (let [s (skip/parse {:skip ["vendored-thing"]})]
    (is (= #{"vendored-thing"} (:dirs s)))
    (is (nil? (skip/reason-for s "vendored-thing")))))

(deftest the-reason-is-carried-so-a-reader-can-ask-whether-it-still-holds-test
  (let [s (skip/parse {:skip [{:dir "clojure-lsp" :reason "vendored upstream"}]})]
    (is (= "vendored upstream" (skip/reason-for s "clojure-lsp")))
    (is (nil? (skip/reason-for s "hive-cache")) "an unlisted dir has no reason")))

(deftest a-blank-reason-reads-as-no-reason-test
  (let [s (skip/parse {:skip [{:dir "x" :reason ""}]})]
    (is (= #{"x"} (:dirs s)))
    (is (nil? (skip/reason-for s "x")))))

;; =============================================================================
;; The file is honoured by discovery, which every command scans through
;; =============================================================================

(defn- workspace!
  "A throwaway workspace with two projects, and SKIP-EDN written to the root
   skip file when given."
  [skip-edn]
  (let [root (fs/create-temp-dir {:prefix "depsolve-skip-test"})]
    (doseq [p ["keep-me" "vendored"]]
      (fs/create-dirs (fs/path root p))
      (spit (str (fs/path root p "deps.edn")) "{:deps {}}")
      (spit (str (fs/path root p "VERSION")) "0.1.0")
      (fs/create-dirs (fs/path root p ".git")))
    (when skip-edn
      (spit (str (fs/path root skip/default-file-name)) (pr-str skip-edn)))
    (str root)))

(deftest with-no-skip-file-every-project-is-scanned-test
  (let [root (workspace! nil)]
    (is (= #{"keep-me" "vendored"}
           (set (map :project (discovery/find-dep-files {:root root})))))))

(deftest a-skipped-directory-is-invisible-to-dep-discovery-test
  (let [root (workspace! {:skip [{:dir "vendored" :reason "not ours to push"}]})]
    (is (= #{"keep-me"}
           (set (map :project (discovery/find-dep-files {:root root}))))
        "the sweep never sees the directory, so it cannot commit into it")))

(deftest a-skipped-directory-is-invisible-to-project-discovery-too-test
  (let [root (workspace! {:skip [{:dir "vendored"}]})
        found (discovery/find-workspace-projects root discovery/default-skip-dirs)]
    (is (= ["keep-me"] (mapv (comp str fs/file-name) found))
        "push-all walks this one, so a skipped dir is never pushed either")))

(deftest the-file-adds-to-the-flag-rather-than-replacing-it-test
  (let [root (workspace! {:skip [{:dir "vendored"}]})]
    (is (empty? (discovery/find-dep-files {:root root :skip-dirs #{"keep-me"}}))
        "--skip-dirs and the file are unioned, so neither silently loses to the other")))

(deftest the-root-itself-is-scanned-even-when-its-name-is-listed-test
  (testing "--root pointing AT a project outranks a skip list meant to bound a sweep"
    (let [root (workspace! nil)
          proj (str (fs/path root "vendored"))]
      (spit (str (fs/path root skip/default-file-name))
            (pr-str {:skip [{:dir "vendored"}]}))
      (is (= ["vendored"] (mapv :project (discovery/find-dep-files {:root proj})))))))

(deftest a-missing-file-excludes-nothing-test
  (is (= #{} (skip/dirs (str (fs/create-temp-dir {:prefix "depsolve-noskip"}))))
      "dirs is safe to union unconditionally"))
