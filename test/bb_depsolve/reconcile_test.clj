(ns bb-depsolve.reconcile-test
  "Tests for bb-depsolve.reconcile."
  (:require [babashka.fs :as fs]
            [bb-depsolve.graph :as g]
            [bb-depsolve.reconcile :as rec]
            [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- node
  [project version]
  {:project project :lib (symbol "io.github.test" project) :dir project
   :release-mode :pinned :version version})

(defn- pin
  [from to version]
  {:project from :dep to :lib (symbol "io.github.test" to)
   :coord :mvn :version version :path (str from "/deps.edn") :scope :runtime})

(def ^:private fleet
  "weave declares 0.2.8 while its consumer already pins 0.2.11."
  (g/dep-graph [(node "weave" "0.2.8") (node "system" "0.1.0")]
               [(pin "system" "weave" "0.2.11")]))

;; =============================================================================
;; Unit — highest-version
;; =============================================================================

(deftest highest-version-orders-by-semver-test
  (is (= "0.10.0" (rec/highest-version ["0.9.0" "0.10.0" "0.2.0"]))
      "not lexicographic")
  (is (= "0.2.0" (rec/highest-version [nil "0.2.0" nil])))
  (is (nil? (rec/highest-version [])))
  (is (nil? (rec/highest-version [nil]))))

;; =============================================================================
;; Unit — drift
;; =============================================================================

(deftest a-consumer-pin-above-the-declared-version-is-drift-test
  (let [d (rec/drift fleet "weave" "0.2.8" nil)]
    (is (= "0.2.8" (:declared d)))
    (is (= "0.2.11" (:highest d)))
    (is (= "0.2.11" (:from-pins d)))
    (is (nil? (:from-tags d)))))

(deftest a-local-tag-above-the-declared-version-is-drift-test
  (let [d (rec/drift fleet "system" "0.1.0" "0.1.8")]
    (is (= "0.1.8" (:highest d)))
    (is (= "0.1.8" (:from-tags d)))))

(deftest the-highest-of-both-sources-wins-test
  (let [d (rec/drift fleet "weave" "0.2.8" "0.2.9")]
    (is (= "0.2.11" (:highest d))
        "the consumer pin is ahead of the local tag")))

(deftest a-declared-version-at-the-top-is-not-drift-test
  (is (nil? (rec/drift fleet "weave" "0.2.11" "0.2.11")))
  (is (nil? (rec/drift fleet "weave" "0.3.0" "0.2.9"))
      "a declared version ahead of every source is not a downgrade candidate"))

(deftest no-evidence-means-no-drift-test
  (is (nil? (rec/drift fleet "system" "0.1.0" nil))))

;; =============================================================================
;; Unit — reading and writing VERSION
;; =============================================================================

(deftest read-declared-ignores-a-missing-or-unparseable-file-test
  (let [dir (str (fs/create-temp-dir {:prefix "bb-depsolve-rec"}))]
    (try
      (is (nil? (rec/read-declared dir)) "no VERSION file")
      (spit (str (fs/path dir "VERSION")) "not-a-version\n")
      (is (nil? (rec/read-declared dir)))
      (spit (str (fs/path dir "VERSION")) "  0.2.8 \n")
      (is (= "0.2.8" (rec/read-declared dir)) "surrounding whitespace is trimmed")
      (finally (fs/delete-tree dir)))))

(deftest apply-drift-rewrites-every-version-file-test
  (let [dir (str (fs/create-temp-dir {:prefix "bb-depsolve-rec"}))]
    (try
      (spit (str (fs/path dir "VERSION")) "0.2.8\n")
      (fs/create-dirs (fs/path dir "sub"))
      (spit (str (fs/path dir "sub" "VERSION")) "0.2.8\n")
      (let [written (rec/apply-drift! dir {:highest "0.2.11"})]
        (is (= 2 (count written)))
        (is (= "0.2.11\n" (slurp (str (fs/path dir "VERSION")))))
        (is (= "0.2.11\n" (slurp (str (fs/path dir "sub" "VERSION"))))
            "nested VERSION files move together, matching bump-cmd"))
      (finally (fs/delete-tree dir)))))

;; =============================================================================
;; Integration — survey
;; =============================================================================

(deftest survey-reports-only-the-drifting-projects-test
  (let [dir (str (fs/create-temp-dir {:prefix "bb-depsolve-rec"}))]
    (try
      (doseq [[p v] {"weave" "0.2.8" "system" "0.1.0"}]
        (fs/create-dirs (fs/path dir p))
        (spit (str (fs/path dir p "VERSION")) (str v "\n")))
      (let [drifts (rec/survey fleet #(str (fs/path dir %)))]
        (is (= ["weave"] (mapv :project drifts))
            "system has no evidence above its declared version")
        (is (= "0.2.11" (:highest (first drifts)))))
      (finally (fs/delete-tree dir)))))

(deftest survey-skips-projects-without-a-version-file-test
  (let [dir (str (fs/create-temp-dir {:prefix "bb-depsolve-rec"}))]
    (try
      (testing "a project with no VERSION file is not a reconcile candidate"
        (is (empty? (rec/survey fleet #(str (fs/path dir %))))))
      (finally (fs/delete-tree dir)))))
