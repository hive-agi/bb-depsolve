(ns bb-depsolve.core.lint-fix-test
  "lint --fix on a dev alias must MOVE it, not re-pin it.

   Re-pinning an alias :override-deps to a published coordinate changes what
   `-M:that-alias` resolves to: the developer asked for the working tree. These
   tests drive lint-cmd over a throwaway workspace, so they touch no registry."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.lint :as lint]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private deps-with-alias
  "{:paths [\"src\"]
 :deps {org.clojure/clojure {:mvn/version \"1.12.5\"}}

 :aliases
 {;; Sibling working trees, for co-development.
  :local-src
  {:override-deps {acme/one {:local/root \"../one\"}}}

  :test {:extra-paths [\"test\"]}}}
")

(defn- project!
  "A git-backed project dir holding `files`, inside a throwaway workspace root."
  [files]
  (let [root (str (fs/create-temp-dir {:prefix "bb-depsolve-lintfix"}))
        dir  (fs/path root "svc")]
    (fs/create-dirs (fs/path dir ".git"))
    (fs/create-dirs (fs/path root "one"))
    (doseq [[name content] files]
      (spit (fs/file (fs/path dir name)) content))
    {:root root :dir (str dir)}))

(defn- fix! [root]
  (with-out-str (lint/lint-cmd {:opts {:root root :org "acme" :fix true}})))

(deftest fix-moves-a-dev-alias-into-local-deps-edn
  (let [{:keys [root dir]} (project! {"deps.edn" deps-with-alias})
        out    (fix! root)
        deps   (slurp (str (fs/path dir "deps.edn")))
        local  (slurp (str (fs/path dir "local.deps.edn")))]
    (testing "the alias leaves deps.edn, comment and all"
      (is (not (str/includes? deps ":local-src")))
      (is (not (str/includes? deps "Sibling working trees")))
      (is (= #{:test} (set (keys (:aliases (edn/read-string deps)))))))
    (testing "it lands under :aliases in local.deps.edn, still an override"
      (is (= "../one" (get-in (edn/read-string local)
                              [:aliases :local-src :override-deps 'acme/one :local/root]))))
    (testing "no re-pin happened: the coordinate is untouched"
      (is (not (str/includes? deps "acme/one")))
      (is (not (str/includes? local ":git/tag")))
      (is (not (str/includes? local ":mvn/version"))))
    (testing "the emitted file still starts with { so -Sdeps reads it as a map"
      (is (str/starts-with? local "{")))
    (testing "and .gitignore learns about it"
      (is (str/includes? (slurp (str (fs/path dir ".gitignore"))) "local.deps.edn")))
    (is (str/includes? out "local.deps.edn :aliases"))))

(deftest fix-merges-into-an-existing-local-deps-edn-instead-of-skipping
  (let [{:keys [root dir]} (project! {"deps.edn" deps-with-alias
                                      "local.deps.edn" "{:deps {acme/kept {:local/root \"../kept\"}}}\n"})
        _      (fix! root)
        local  (edn/read-string (slurp (str (fs/path dir "local.deps.edn"))))]
    (testing "an existing override file is merged, not left behind"
      (is (= "../kept" (get-in local [:deps 'acme/kept :local/root])))
      (is (= "../one" (get-in local [:aliases :local-src :override-deps 'acme/one :local/root]))))))

(deftest lint-does-not-flag-a-path-inside-the-same-repo
  (let [{:keys [root dir]} (project! {"deps.edn" "{:deps {acme/sub {:local/root \"lib\"}}}\n"})
        _   (fs/create-dirs (fs/path dir "lib"))
        out (with-out-str (lint/lint-cmd {:opts {:root root :org "acme"}}))]
    (is (str/includes? out "All clean"))
    (is (str/includes? out "not flagged"))
    (testing "and --fix leaves such a file alone"
      (fix! root)
      (is (str/includes? (slurp (str (fs/path dir "deps.edn"))) ":local/root \"lib\"")))))

(deftest lint-does-not-flag-a-dep-file-outside-any-git-repo
  (let [root (str (fs/create-temp-dir {:prefix "bb-depsolve-nogit"}))]
    (spit (fs/file (fs/path root "deps.edn"))
          "{:deps {acme/sib {:local/root \"../sibling\"}}}\n")
    (let [out (with-out-str (lint/lint-cmd {:opts {:root root :org "acme"}}))]
      (is (str/includes? out "All clean"))
      (is (str/includes? out "no git repo")))))
