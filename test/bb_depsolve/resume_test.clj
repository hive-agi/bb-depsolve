(ns bb-depsolve.resume-test
  "Tests for bb-depsolve.release.resume and the interpreter's resume seam."
  (:require [babashka.fs :as fs]
            [bb-depsolve.cascade.plan :as cas]
            [bb-depsolve.release.exec :as exec]
            [bb-depsolve.graph.dag :as g]
            [bb-depsolve.release.port :as p]
            [bb-depsolve.release.resume :as resume]
            [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- node
  [project mode version]
  {:project project :lib (symbol "io.github.test" project) :dir project
   :release-mode mode :version version})

(defn- pin
  [from to coord version]
  {:project from :dep to :lib (symbol "io.github.test" to)
   :coord coord :version version :path (str from "/deps.edn") :scope :runtime})

(def ^:private fleet
  (g/dep-graph [(node "weave" :pinned "0.3.0")
                (node "mid" :rolling "0.2.4")
                (node "top" :pinned "0.1.0")]
               [(pin "mid" "weave" :mvn "0.3.0")
                (pin "top" "mid" :git "v0.2.4")]))

(def ^:private plan (cas/plan-cascade fleet #{"weave"}))

(def ^:private quiet
  {:emit (constantly nil) :await-opts {:emit (constantly nil)}})

(defn- with-temp-root
  [f]
  (let [dir (str (fs/create-temp-dir {:prefix "bb-depsolve-resume"}))]
    (try (f dir)
         (finally (fs/delete-tree dir)))))

;; =============================================================================
;; Unit — the checkpoint file
;; =============================================================================

(deftest a-saved-run-round-trips-test
  (with-temp-root
    (fn [root]
      (is (nil? (resume/load-run root)) "no checkpoint before the first save")
      (let [run {:status :aborted :waves [] :released {"weave" "0.3.1"}}]
        (resume/save! root run)
        (is (= run (resume/load-run root)))
        (is (true? (resume/clear! root)))
        (is (nil? (resume/load-run root)))))))

(deftest an-unreadable-checkpoint-is-treated-as-absent-test
  (with-temp-root
    (fn [root]
      (let [path (resume/record-path root)]
        (fs/create-dirs (fs/parent path))
        (spit path "{:this is not readable edn")
        (is (nil? (resume/load-run root))
            "a corrupt checkpoint must not abort the run")))))

;; =============================================================================
;; Unit — narrowing the plan
;; =============================================================================

(deftest remaining-drops-released-steps-and-empty-waves-test
  (let [todo (resume/remaining plan {"weave" "0.3.1"})]
    (is (= [["mid"] ["top"]]
           (mapv #(mapv :project (:steps %)) (:waves todo)))
        "the finished wave disappears entirely")
    (is (= [1 2] (mapv :index (:waves todo)))
        "surviving waves keep their original index")))

(deftest remaining-of-a-fully-released-plan-is-empty-test
  (is (empty? (:waves (resume/remaining plan {"weave" "0.3.1"
                                              "mid" "0.2.5"
                                              "top" "0.1.1"})))))

(deftest remaining-without-a-checkpoint-is-the-whole-plan-test
  (is (= (:waves plan) (:waves (resume/remaining plan {})))))

;; =============================================================================
;; Integration — the interpreter honours a resume
;; =============================================================================

(deftest a-resumed-run-does-not-re-release-what-already-published-test
  (let [port (p/memory-port)
        done {"weave" "0.3.1"}
        todo (resume/remaining plan done)
        result (exec/run-plan! port port todo (assoc quiet :released done))
        run (:ok result)]
    (is (= :complete (:status run)))
    (is (= {"weave" "0.3.1" "mid" "0.2.5" "top" "0.1.1"} (:released run))
        "the seeded release is carried into the final record")
    (is (= ["mid" "top"]
           (mapv second (filter #(= :release (first %)) (:log @(:state port)))))
        "weave is never released a second time")))

(deftest a-resumed-run-still-repins-against-the-seeded-versions-test
  (testing "a pin whose target released in the earlier run is filled from the seed"
    (let [port (p/memory-port)
          done {"weave" "0.3.1"}
          todo (resume/remaining plan done)
          result (exec/run-plan! port port todo (assoc quiet :released done))
          mid (first (:steps (first (:waves (:ok result)))))]
      (is (= [{:dep "weave" :lib 'io.github.test/weave :coord :mvn
               :path "mid/deps.edn" :from "0.3.0" :to "0.3.1"}]
             (:pin-updates mid))))))

;; =============================================================================
;; Integration — checkpointing
;; =============================================================================

(deftest every-wave-checkpoints-the-partial-run-test
  (let [port (p/memory-port)
        seen (atom [])
        _ (exec/run-plan! port port plan (assoc quiet :on-wave #(swap! seen conj %)))]
    (is (= 3 (count @seen)) "one checkpoint per wave")
    (is (= [:running :running :running] (mapv :status @seen)))
    (is (= [{"weave" "0.3.1"}
            {"weave" "0.3.1" "mid" "0.2.5"}
            {"weave" "0.3.1" "mid" "0.2.5" "top" "0.1.1"}]
           (mapv :released @seen))
        "each checkpoint carries everything released so far")))

(deftest an-aborted-run-checkpoints-what-it-finished-test
  (let [port (p/memory-port {:fail #{"mid"}})
        seen (atom [])
        result (exec/run-plan! port port plan (assoc quiet :on-wave #(swap! seen conj %)))]
    (is (= :exec/step-failed (:kind (:error result))))
    (is (= :aborted (:status (last @seen))))
    (is (= {"weave" "0.3.1"} (:released (last @seen)))
        "the checkpoint is exactly what a re-run may skip")))

(deftest a-checkpointed-abort-resumes-to-completion-test
  (with-temp-root
    (fn [root]
      (let [failing (p/memory-port {:fail #{"mid"}})
            _ (exec/run-plan! failing failing plan
                              (assoc quiet :on-wave #(resume/save! root %)))
            checkpoint (resume/load-run root)
            done (resume/released checkpoint)
            todo (resume/remaining plan done)
            healthy (p/memory-port)
            result (exec/run-plan! healthy healthy todo (assoc quiet :released done))]
        (is (= {"weave" "0.3.1"} done))
        (is (= :complete (:status (:ok result))))
        (is (= ["mid" "top"]
               (mapv second (filter #(= :release (first %)) (:log @(:state healthy)))))
            "the second run picks up exactly where the first stopped")))))
