(ns bb-depsolve.core.bump-test
  "Tests for the bump command: flag mapping, the bump plan, and the --apply gate."
  (:require [babashka.fs :as fs]
            [babashka.process :as proc]
            [bb-depsolve.core.bump :as bump]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- project!
  "A throwaway project directory holding VERSION."
  [version]
  (let [dir (str (fs/create-temp-dir {:prefix "bb-depsolve-bump"}))]
    (spit (str (fs/path dir "VERSION")) (str version "\n"))
    dir))

(defn- version-of [dir]
  (str/trim (slurp (str (fs/path dir "VERSION")))))

(defn- recording-sh
  "proc/sh stand-in: records the argv and reports success."
  [calls]
  (fn [args & _] (swap! calls conj (vec args)) {:exit 0 :out "" :err ""}))

(defn- git-verbs [calls]
  (mapv #(nth % 3 nil) @calls))

;; =============================================================================
;; Unit — bump-level
;; =============================================================================

(deftest bump-level-maps-each-flag-to-its-own-segment-test
  (is (= :major (bump/bump-level {:major true})) "--major bumps the major segment")
  (is (= :minor (bump/bump-level {:minor true})) "--minor bumps the minor segment")
  (is (= :patch (bump/bump-level {})) "patch is the default")
  (is (= :major (bump/bump-level {:stable true}))
      "--stable is a major promotion — on a 0.x version that is 1.0.0"))

(deftest bump-level-takes-the-strongest-flag-test
  (is (= :major (bump/bump-level {:major true :minor true})))
  (is (= :major (bump/bump-level {:stable true :minor true}))))

;; =============================================================================
;; Unit — plan-bump
;; =============================================================================

(deftest plan-bump-describes-the-resulting-version-test
  (is (= {:current "0.4.2" :level :patch :new-version "0.4.3" :new-tag "v0.4.3"}
         (bump/plan-bump "0.4.2" {})))
  (is (= {:current "0.4.2" :level :minor :new-version "0.5.0" :new-tag "v0.5.0"}
         (bump/plan-bump "0.4.2" {:minor true})))
  (is (= {:current "0.4.2" :level :major :new-version "1.0.0" :new-tag "v1.0.0"}
         (bump/plan-bump "0.4.2" {:major true})))
  (is (= "1.0.0" (:new-version (bump/plan-bump "0.4.2" {:stable true})))
      "--stable promotes a pre-1.0 project to 1.0.0"))

(deftest plan-bump-is-nil-for-an-unparseable-version-test
  (is (nil? (bump/plan-bump "not-a-version" {})))
  (is (nil? (bump/plan-bump "" {}))))

;; =============================================================================
;; Unit — bump-cmd, the --apply gate
;; =============================================================================

(deftest bump-cmd-without-apply-writes-nothing-test
  (let [dir (project! "0.4.2")
        calls (atom [])
        out (with-out-str
              (with-redefs [proc/sh (recording-sh calls)]
                (bump/bump-cmd {:opts {:root dir}})))]
    (is (= "0.4.2" (version-of dir)) "the VERSION file is left alone")
    (is (= [] @calls) "no git command runs — nothing is committed, tagged or pushed")
    (is (str/includes? out "0.4.3") "the planned version is still reported")
    (is (str/includes? out "--apply") "and the dry run says how to execute it")))

(deftest bump-cmd-with-apply-writes-tags-and-pushes-test
  (let [dir (project! "0.4.2")
        calls (atom [])]
    (with-out-str
      (with-redefs [proc/sh (recording-sh calls)]
        (bump/bump-cmd {:opts {:root dir :apply true}})))
    (is (= "0.4.3" (version-of dir)))
    (is (= ["add" "commit" "tag" "push" "push"] (git-verbs calls)))
    (is (some #(= "v0.4.3" (last %)) @calls) "the new tag reaches git")))

(deftest bump-cmd-applies-the-requested-level-test
  (testing "--minor yields a minor bump, not a patch"
    (let [dir (project! "0.4.2")]
      (with-out-str
        (with-redefs [proc/sh (recording-sh (atom []))]
          (bump/bump-cmd {:opts {:root dir :apply true :minor true}})))
      (is (= "0.5.0" (version-of dir)))))
  (testing "--major yields a major bump, not a minor"
    (let [dir (project! "0.4.2")]
      (with-out-str
        (with-redefs [proc/sh (recording-sh (atom []))]
          (bump/bump-cmd {:opts {:root dir :apply true :major true}})))
      (is (= "1.0.0" (version-of dir))))))

(deftest bump-cmd-updates-nested-version-files-test
  (let [dir (project! "0.4.2")
        nested (str (fs/path dir "sub" "VERSION"))]
    (fs/create-dirs (fs/path dir "sub"))
    (spit nested "0.4.2\n")
    (with-out-str
      (with-redefs [proc/sh (recording-sh (atom []))]
        (bump/bump-cmd {:opts {:root dir :apply true}})))
    (is (= "0.4.3" (str/trim (slurp nested))))))
