(ns bb-depsolve.git-port-test
  "Tests for bb-depsolve.release.git-port against throwaway local repos."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.release.git-port :as gp]
            [bb-depsolve.release.port :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private weave 'io.github.test/weave)

(defn- git!
  [dir & args]
  (apply git/git dir args))

(defn- scratch
  "A git repo with one committed deps.edn pinning weave, plus a bare remote.
   => {:dir :remote :deps}"
  [{:keys [version-file?] :or {version-file? true}}]
  (let [base (str (fs/create-temp-dir {:prefix "bb-depsolve-gitport"}))
        remote (str (fs/path base "remote.git"))
        dir (str (fs/path base "top"))
        deps (str (fs/path dir "deps.edn"))]
    (fs/create-dirs dir)
    (git! base "init" "--bare" "--initial-branch=main" remote)
    (git! dir "init" "--initial-branch=main")
    (git! dir "config" "user.email" "test@example.com")
    (git! dir "config" "user.name" "test")
    (spit deps (str "{:deps {io.github.test/weave {:mvn/version \"0.3.0\"}\n"
                    "        io.github.test/spi {:git/tag \"v0.1.0\" "
                    ":git/sha \"aaaaaaa\"}}}\n"))
    (spit (str (fs/path dir "version.edn")) "{:lib io.github.test/top :minor 4}\n")
    (when version-file? (spit (str (fs/path dir "VERSION")) "0.1.0\n"))
    (git! dir "add" ".")
    (git! dir "commit" "-m" "init")
    (git! dir "remote" "add" "origin" remote)
    (git! dir "push" "-u" "origin" "main")
    {:dir dir :remote remote :deps deps}))

(defn- step
  [dir deps mode next-version]
  {:project "top" :lib 'io.github.test/top :dir dir :role :consumer
   :release-mode mode :current-version "0.1.0"
   :bump-kind (when (= :pinned mode) :patch) :next-version next-version
   :pin-updates [{:dep "weave" :lib weave :coord :mvn :path deps
                  :from "0.3.0" :to "0.3.1"}]})

(defn- tags
  [dir]
  (set (remove str/blank? (str/split-lines (:out (git! dir "tag" "-l"))))))

(deftest apply-pin-rewrites-each-coordinate-in-its-own-shape-test
  (let [content (str "{:deps {io.github.test/weave {:mvn/version \"0.3.0\"}\n"
                     "        io.github.test/spi {:git/tag \"v0.1.0\" "
                     ":git/sha \"aaaaaaa\"}}}")]
    (is (str/includes? (gp/apply-pin content {:lib weave :coord :mvn :to "0.3.1"})
                       "\"0.3.1\""))
    (let [rewritten (gp/apply-pin content {:lib 'io.github.test/spi :coord :git
                                           :to "v0.2.0" :sha "bbbbbbb"})]
      (is (str/includes? rewritten "\"v0.2.0\""))
      (is (str/includes? rewritten "\"bbbbbbb\"")))))

(deftest a-git-pin-without-a-sha-is-left-alone-test
  (let [content "{:deps {io.github.test/spi {:git/tag \"v0.1.0\" :git/sha \"aaaaaaa\"}}}"]
    (is (= content (gp/apply-pin content {:lib 'io.github.test/spi :coord :git
                                          :to "v0.2.0" :sha nil}))
        "rewriting a tag without its sha would publish an inconsistent coordinate")))

(deftest an-unresolvable-tag-fails-loudly-test
  (let [{:keys [error]} (gp/resolve-shas [{:lib 'not-a-github/lib :coord :git
                                           :to "v9.9.9"}])]
    (is (= :git-port/unresolved-tag (:kind error)))
    (is (= [{:lib 'not-a-github/lib :to "v9.9.9"}] (:pins error)))))

(deftest maven-pins-need-no-resolution-test
  (is (= [{:lib weave :coord :mvn :to "0.3.1"}]
         (:ok (gp/resolve-shas [{:lib weave :coord :mvn :to "0.3.1"}])))))

(deftest sync-pins-rewrites-and-commits-test
  (let [{:keys [dir deps]} (scratch {})
        port (gp/git-port {:remote "origin"})
        {:keys [ok]} (p/sync-pins! port (step dir deps :pinned "0.1.1"))]
    (is (= #{deps} (set (:paths ok))))
    (is (str/includes? (slurp deps) "\"0.3.1\""))
    (is (str/blank? (:out (git! dir "status" "--porcelain")))
        "the rewrite is committed, not left dirty")
    (is (str/includes? (:out (git! dir "log" "-1" "--format=%s"))
                       gp/pin-commit-message))))

(deftest sync-pins-in-rehearsal-mode-touches-nothing-test
  (let [{:keys [dir deps]} (scratch {})
        port (gp/git-port {:write? false})
        {:keys [ok]} (p/sync-pins! port (step dir deps :pinned "0.1.1"))]
    (is (= #{deps} (set (:paths ok))) "it still reports what would change")
    (is (str/includes? (slurp deps) "\"0.3.0\"") "but the file is untouched")
    (is (= "init" (str/trim (:out (git! dir "log" "-1" "--format=%s")))))))

(deftest a-pin-with-no-target-is-skipped-test
  (let [{:keys [dir deps]} (scratch {})
        port (gp/git-port {:write? false})
        blank (assoc-in (step dir deps :pinned "0.1.1") [:pin-updates 0 :to] nil)
        {:keys [ok]} (p/sync-pins! port blank)]
    (is (empty? (:paths ok)))
    (is (= ["weave"] (mapv :dep (:skipped ok))))))

(deftest a-pinned-release-writes-tags-and-pushes-test
  (let [{:keys [dir remote deps]} (scratch {})
        port (gp/git-port {})]
    (p/sync-pins! port (step dir deps :pinned "0.1.1"))
    (let [{:keys [ok]} (p/release! port (step dir deps :pinned "0.1.1"))]
      (is (= {:project "top" :release-mode :pinned :version "0.1.1" :tag "v0.1.1"} ok))
      (is (= "0.1.1" (str/trim (slurp (str (fs/path dir "VERSION"))))))
      (is (contains? (tags dir) "v0.1.1"))
      (testing "and the remote has both the commit and the tag"
        (is (str/includes? (:out (git! remote "tag" "-l")) "v0.1.1"))
        (is (str/includes? (:out (git! remote "log" "-1" "--format=%s"))
                           "release: v0.1.1"))))))

(deftest a-pinned-release-without-a-target-version-is-refused-test
  (let [{:keys [dir deps]} (scratch {})
        {:keys [error]} (p/release! (gp/git-port {:write? false})
                                    (step dir deps :pinned nil))]
    (is (= :git-port/no-target-version (:kind error)))))

(deftest a-rolling-release-pushes-and-reports-the-minted-version-test
  (let [{:keys [dir remote deps]} (scratch {:version-file? false})
        port (gp/git-port {})]
    (p/sync-pins! port (step dir deps :rolling nil))
    (let [{:keys [ok]} (p/release! port (step dir deps :rolling nil))]
      (is (= :rolling (:release-mode ok)))
      (is (nil? (:tag ok)) "a rolling release carries no tag")
      (is (= "0.4.2" (:version ok))
          "version.edn :minor 4 plus the two commits now on HEAD")
      (is (str/includes? (:out (git! remote "log" "-1" "--format=%s"))
                         gp/pin-commit-message)))))

(deftest an-unknown-release-mode-is-refused-test
  (let [{:keys [dir deps]} (scratch {})
        {:keys [error]} (p/release! (gp/git-port {:write? false})
                                    (assoc (step dir deps :pinned "0.1.1")
                                           :release-mode :magic))]
    (is (= :git-port/unknown-release-mode (:kind error)))))

(deftest a-failing-push-surfaces-as-an-error-test
  (let [{:keys [dir deps]} (scratch {})
        port (gp/git-port {:remote "nowhere"})
        {:keys [error]} (p/release! port (step dir deps :pinned "0.1.1"))]
    (is (= :git-port/git-failed (:kind error)))
    (is (= ["push" "nowhere" "HEAD"] (:args error))
        "it stops at the first failing command, after the local tag")))
