(ns bb-depsolve.survey-test
  "Tests for bb-depsolve.release.survey. Every repo is a throwaway one in a
   temp dir; the survey itself only reads."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.release.survey :as sv]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private clean-tracking
  (str "# branch.oid 1111111111111111111111111111111111111111\n"
       "# branch.head main\n"
       "# branch.upstream origin/main\n"
       "# branch.ab +2 -1\n"))

(def ^:private dirty-mixed
  (str clean-tracking
       "1 .M N... 100644 100644 100644 aaaa bbbb src/a.clj\n"
       "2 R. N... 100644 100644 100644 aaaa bbbb R100 new.clj\told.clj\n"
       "u UU N... 100644 100644 100644 100644 aaaa bbbb cccc conflict.clj\n"
       "? scratch.txt\n"
       "? other.txt\n"
       "! ignored.log\n"))

(def ^:private no-upstream
  (str "# branch.oid 1111111111111111111111111111111111111111\n"
       "# branch.head topic\n"))

(def ^:private detached
  (str "# branch.oid 1111111111111111111111111111111111111111\n"
       "# branch.head (detached)\n"))

(def ^:private unborn
  (str "# branch.oid (initial)\n"
       "# branch.head main\n"))

(def ^:private gen-status
  "Well-formed to hostile status output: real header lines, entries, blanks
   and free text."
  (gen/let [lines (gen/vector
                   (gen/one-of
                    [(gen/elements (str/split-lines dirty-mixed))
                     (gen/elements (str/split-lines no-upstream))
                     (gen/elements (str/split-lines detached))
                     (gen/return "")
                     gen/string-alphanumeric])
                   0 12)]
    (str/join "\n" lines)))

(defn- status-shape?
  [{:keys [branch upstream ahead behind dirty untracked]}]
  (and (or (nil? branch) (string? branch))
       (or (nil? upstream) (string? upstream))
       (or (nil? ahead) (nat-int? ahead))
       (or (nil? behind) (nat-int? behind))
       (nat-int? dirty)
       (nat-int? untracked)))

;; =============================================================================
;; Trifecta: the status parser
;; =============================================================================

(tri/deftrifecta parse-status-v2-trifecta
  bb-depsolve.release.survey/parse-status-v2
  {:golden-path "test/golden/bb-depsolve/survey-parse-status-v2.edn"
   :cases       {:tracking-clean clean-tracking
                 :dirty-mixed    dirty-mixed
                 :no-upstream    no-upstream
                 :detached       detached
                 :unborn         unborn
                 :empty          ""}
   :gen         gen-status
   :pred        status-shape?
   :num-tests   200
   :mutations   [["counts-untracked-as-dirty"
                  (fn [output]
                    (let [lines (remove #(or (str/blank? %) (str/starts-with? % "#"))
                                        (str/split-lines (or output "")))]
                      {:branch nil :upstream nil :ahead nil :behind nil
                       :dirty (count lines) :untracked 0}))]
                 ["reports-detached-head-as-a-branch"
                  (fn [output]
                    (let [head (some #(when (str/starts-with? % "# branch.head ")
                                        (subs % (count "# branch.head ")))
                                     (str/split-lines (or output "")))]
                      {:branch head :upstream nil :ahead nil :behind nil
                       :dirty 0 :untracked 0}))]]})

;; =============================================================================
;; Pure parsers
;; =============================================================================

(deftest remote-urls-keep-every-remote-and-the-first-url-test
  (is (= {"origin" "git@github.com:o/r.git" "mirror" "https://example.org/r.git"}
         (sv/parse-remote-urls
          (str "remote.origin.url git@github.com:o/r.git\n"
               "remote.mirror.url https://example.org/r.git\n"
               "remote.origin.url ignored-second-url\n"))))
  (is (= {} (sv/parse-remote-urls "")))
  (is (= {} (sv/parse-remote-urls nil)))
  (is (= {"a.b" "u"} (sv/parse-remote-urls "remote.a.b.url u\n"))
      "a remote name may contain dots"))

(deftest remote-head-names-the-default-branch-test
  (is (= "main" (sv/parse-remote-head "origin" "origin/main\n")))
  (is (= "feature/x" (sv/parse-remote-head "origin" "origin/feature/x")))
  (is (nil? (sv/parse-remote-head "origin" "")))
  (is (nil? (sv/parse-remote-head "origin" "upstream/main"))))

(deftest origin-is-the-default-remote-when-present-test
  (is (= "origin" (sv/default-remote {"zzz" "u" "origin" "u"})))
  (is (= "aaa" (sv/default-remote (sorted-map "bbb" "u" "aaa" "u"))))
  (is (nil? (sv/default-remote {}))))

;; =============================================================================
;; Real (temp) repos
;; =============================================================================

(def ^:private hermetic-env
  "The host's git config must not reach a fixture repo."
  {"GIT_CONFIG_GLOBAL" "/dev/null" "GIT_CONFIG_SYSTEM" "/dev/null"})

(defn- git!
  [dir & args]
  (let [r (apply git/git dir args)]
    (when-not (zero? (long (:exit r)))
      (throw (ex-info (str "fixture git failed: " args) r)))
    r))

(defn- init-repo!
  "A repo named NAME under BASE with one commit and a VERSION file."
  [base name]
  (let [dir (str (fs/path base name))]
    (fs/create-dirs dir)
    (git! dir "init" "--initial-branch=main")
    (git! dir "config" "user.email" "test@example.com")
    (git! dir "config" "user.name" "test")
    (git! dir "config" "commit.gpgsign" "false")
    (git! dir "config" "tag.gpgsign" "false")
    (spit (str (fs/path dir "VERSION")) "0.1.0\n")
    (spit (str (fs/path dir "f.txt")) "one\n")
    (git! dir "add" ".")
    (git! dir "commit" "-m" "init")
    dir))

(defmacro ^:private with-base
  [[sym] & body]
  `(let [~sym (str (fs/create-temp-dir {:prefix "bb-depsolve-survey"}))]
     (try ~@body
          (finally (fs/delete-tree ~sym)))))

(defn- row [rows project] (first (filter #(= project (:project %)) rows)))

(deftest a-repo-with-no-upstream-no-tag-and-no-remote-is-honest-test
  (with-base [base]
    (init-repo! base "lone")
    (let [r (row (sv/survey-workspace {:root base}) "lone")]
      (is (= "main" (:branch r)))
      (is (nil? (:upstream r)))
      (is (nil? (:ahead r)))
      (is (nil? (:behind r)))
      (is (nil? (:last-tag r)))
      (is (= {} (:remotes r)))
      (is (nil? (:default-branch r)))
      (is (= "0.1.0" (:version r)))
      (is (= 0 (:dirty r)))
      (is (= 0 (:untracked r))))))

(deftest tags-dirty-files-and-untracked-files-are-counted-test
  (with-base [base]
    (let [dir (init-repo! base "busy")]
      (git! dir "tag" "-a" "v0.1.0" "-m" "release")
      (spit (str (fs/path dir "f.txt")) "two\n")
      (spit (str (fs/path dir "new.txt")) "x\n")
      (let [r (row (sv/survey-workspace {:root base}) "busy")]
        (is (= "v0.1.0" (:last-tag r)))
        (is (= 1 (:dirty r)))
        (is (= 1 (:untracked r)))))))

(deftest ahead-and-behind-are-read-against-the-tracked-upstream-test
  (with-base [base]
    (let [dir (init-repo! base "tracked")
          bare (str (fs/path base "remote.git"))
          peer (str (fs/path base "peer"))]
      (git! base "init" "--bare" "--initial-branch=main" bare)
      (git! dir "remote" "add" "origin" bare)
      (git! dir "push" "-u" "origin" "main")
      (git! base "clone" bare peer)
      (git! peer "config" "user.email" "test@example.com")
      (git! peer "config" "user.name" "test")
      (git! peer "config" "commit.gpgsign" "false")
      (spit (str (fs/path peer "p.txt")) "p\n")
      (git! peer "add" ".")
      (git! peer "commit" "-m" "peer commit")
      (git! peer "push" "origin" "main")
      (spit (str (fs/path dir "mine.txt")) "m\n")
      (git! dir "add" ".")
      (git! dir "commit" "-m" "local commit")
      (git! dir "fetch" "origin")
      (git! dir "remote" "set-head" "origin" "main")
      (let [r (row (sv/survey-workspace {:root base}) "tracked")]
        (is (= "origin/main" (:upstream r)))
        (is (= 1 (:ahead r)))
        (is (= 1 (:behind r)))
        (is (= {"origin" bare} (:remotes r)))
        (is (= "main" (:default-branch r)))))))

(deftest several-remotes-are-all-reported-test
  (with-base [base]
    (let [dir (init-repo! base "multi")]
      (git! dir "remote" "add" "origin" "git@github.com:o/r.git")
      (git! dir "remote" "add" "mirror" "https://example.org/r.git")
      (let [r (row (sv/survey-workspace {:root base}) "multi")]
        (is (= {"origin" "git@github.com:o/r.git"
                "mirror" "https://example.org/r.git"}
               (:remotes r)))
        (is (nil? (:default-branch r))
            "no refs/remotes/origin/HEAD locally, and the survey never asks a forge")))))

(deftest a-detached-head-has-no-branch-test
  (with-base [base]
    (let [dir (init-repo! base "detached")]
      (git! dir "checkout" "--detach")
      (let [r (row (sv/survey-workspace {:root base}) "detached")]
        (is (nil? (:branch r)))
        (is (nil? (:upstream r)))))))

(deftest the-workspace-survey-is-sorted-and-skips-non-repos-test
  (with-base [base]
    (init-repo! base "zeta")
    (init-repo! base "alpha")
    (fs/create-dirs (fs/path base "plain"))
    (spit (str (fs/path base "plain" "VERSION")) "0.0.1\n")
    (is (= ["alpha" "zeta"] (mapv :project (sv/survey-workspace {:root base}))))))

(deftest the-survey-never-mutates-a-repo-test
  (with-base [base]
    (let [dir (init-repo! base "pure")]
      (spit (str (fs/path dir "f.txt")) "dirty\n")
      (let [before (:out (git! dir "status" "--porcelain=v2" "--branch"))
            head (:out (git! dir "rev-parse" "HEAD"))]
        (sv/survey-workspace {:root base})
        (is (= before (:out (git! dir "status" "--porcelain=v2" "--branch"))))
        (is (= head (:out (git! dir "rev-parse" "HEAD"))))))))

(deftest every-git-call-is-a-read-test
  (with-base [base]
    (init-repo! base "probe")
    (let [seen (atom [])
          spy (fn [dir & args] (swap! seen conj (vec args)) (apply git/git dir args))]
      (sv/survey-workspace {:root base :git-fn spy})
      (is (seq @seen))
      (is (every? #(contains? #{"status" "config" "describe" "symbolic-ref"} (first %))
                  @seen))
      (is (every? #(not-any? #{"--add" "--unset" "--set-upstream"} %) @seen)))))

(deftest an-unreadable-repo-becomes-an-error-row-test
  (with-base [base]
    (let [dir (init-repo! base "broken")
          failing (fn [_dir & _args] {:exit 128 :out "" :err "fatal"})
          r (sv/read-repo failing dir)]
      (is (string? (:error r)))
      (is (= "broken" (:project r))))))

(deftest a-throwing-runner-becomes-an-error-row-test
  (let [boom (fn [& _] (throw (ex-info "no git here" {})))
        r (sv/read-repo boom "/tmp/whatever")]
    (is (= "no git here" (:error r)))))
