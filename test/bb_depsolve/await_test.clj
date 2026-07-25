(ns bb-depsolve.await-test
  "Tests for bb-depsolve.await."
  (:require [bb-depsolve.release.await :as aw]
            [bb-depsolve.release.port :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private weave 'io.github.test/weave)
(def ^:private dsl 'io.github.test/dsl)

(defn- clock
  "Test clock advanced only by the sleeps the loop performs.
   => {:now :sleep :elapsed}."
  []
  (let [t (atom 0)]
    {:now #(deref t)
     :sleep #(swap! t + (long %))
     :elapsed #(deref t)}))

(defn- silent [] (fn [_entries _elapsed] nil))

(defn- recorder
  "Emitter capturing every frame. => [emit frames]."
  []
  (let [frames (atom [])]
    [(fn [entries elapsed] (swap! frames conj [elapsed (mapv :state entries)]))
     frames]))

(defn- step
  [project lib next-version]
  {:project project :lib lib :dir project :role :seed
   :release-mode :pinned :current-version "0.3.0"
   :bump-kind :patch :next-version next-version :pin-updates []})

(def ^:private directive
  {:mode :wait
   :timeout-ms 60000
   :libs [{:lib weave :newer-than "0.3.0" :expect "0.3.1"}
          {:lib dsl :newer-than "0.5.8" :expect "0.5.9"}]})

;; =============================================================================
;; Unit — backoff
;; =============================================================================

(deftest backoff-doubles-then-holds-at-the-cap-test
  (is (= [1000 2000 4000 8000 8000 8000]
         (take 6 (aw/backoff-delays {:start-ms 1000 :cap-ms 8000})))))

;; =============================================================================
;; Unit — short circuits
;; =============================================================================

(deftest skip-mode-never-polls-test
  (let [registry (p/memory-port)
        result (aw/await-wave! registry (assoc directive :mode :skip)
                               {:emit (silent)})]
    (is (= :skip (:mode (:ok result))))
    (is (= 0 (:elapsed-ms (:ok result))))
    (is (empty? (:pending @(:state registry))))))

(deftest an-empty-wave-is-a-no-op-test
  (is (= [] (:libs (:ok (aw/await-wave! (p/memory-port)
                                        (assoc directive :libs [])
                                        {:emit (silent)}))))))

;; =============================================================================
;; Unit — polling to completion
;; =============================================================================

(deftest a-wave-resolves-once-every-artifact-publishes-test
  (let [registry (p/memory-port {:publish-after 3})
        {:keys [now sleep]} (clock)]
    (p/release! registry (step "weave" weave "0.3.1"))
    (p/release! registry (step "dsl" dsl "0.5.9"))
    (let [result (aw/await-wave! registry directive
                                 {:emit (silent) :now now :sleep sleep
                                  :backoff {:start-ms 1000 :cap-ms 4000}})]
      (is (:ok result))
      (is (= :wait (:mode (:ok result))))
      (is (every? aw/resolved? (:libs (:ok result))))
      (is (= 3000 (:elapsed-ms (:ok result)))
          "two sleeps of 1000 and 2000 before the third poll succeeds"))))

(deftest a-partially-published-wave-keeps-waiting-test
  (let [registry (p/memory-port {:publish-after 2})
        {:keys [now sleep]} (clock)]
    (p/release! registry (step "weave" weave "0.3.1"))
    (let [result (aw/await-wave! registry (assoc directive :timeout-ms 5000)
                                 {:emit (silent) :now now :sleep sleep
                                  :backoff {:start-ms 1000 :cap-ms 1000}})]
      (is (not (:ok result)))
      (is (= [dsl] (:unresolved (:error result)))
          "weave published; dsl was never released"))))

;; =============================================================================
;; Unit — timeout is loud
;; =============================================================================

(deftest a-timeout-names-every-unresolved-lib-test
  (let [{:keys [now sleep]} (clock)
        result (aw/await-wave! (p/memory-port) (assoc directive :timeout-ms 3000)
                               {:emit (silent) :now now :sleep sleep
                                :backoff {:start-ms 1000 :cap-ms 1000}})
        error (:error result)]
    (is (= :await/timeout (:kind error)))
    (is (= #{weave dsl} (set (:unresolved error))))
    (is (>= (long (:elapsed-ms error)) 3000))
    (testing "and says so in words"
      (let [text (aw/format-timeout error)]
        (is (str/includes? text "await timed out"))
        (is (str/includes? text (str weave)))
        (is (str/includes? text "--no-wait"))))))

;; =============================================================================
;; Unit — visible progress
;; =============================================================================

(deftest every-poll-emits-a-frame-test
  (let [registry (p/memory-port {:publish-after 2})
        {:keys [now sleep]} (clock)
        [emit frames] (recorder)]
    (p/release! registry (step "weave" weave "0.3.1"))
    (p/release! registry (step "dsl" dsl "0.5.9"))
    (aw/await-wave! registry directive
                    {:emit emit :now now :sleep sleep
                     :backoff {:start-ms 1000 :cap-ms 1000}})
    (is (= [[0 [:pending :pending]] [1000 [:resolved :resolved]]] @frames))))

(deftest a-frame-shows-one-line-per-lib-plus-a-header-test
  (let [entries [{:lib weave :expect "0.3.1" :state :pending}
                 {:lib dsl :newer-than "0.5.8" :state :resolved}]
        lines (aw/render-lines entries 7000)]
    (is (= 3 (count lines)))
    (is (str/includes? (first lines) "waiting on 1 of 2"))
    (is (str/includes? (first lines) "7s elapsed"))
    (is (str/includes? (nth lines 1) "published") "dsl sorts first and is done")
    (is (str/includes? (nth lines 2) "= 0.3.1") "weave states what it waits for")))

(deftest a-finished-frame-says-so-test
  (is (str/includes? (first (aw/render-lines [{:lib weave :state :resolved}] 1000))
                     "all 1 artifact(s) published")))

(deftest the-plain-emitter-prints-only-transitions-test
  (let [emit (aw/plain-emitter)
        entries [{:lib weave :state :pending}]
        out (with-out-str
              (emit entries 0)
              (emit entries 1000)
              (emit [{:lib weave :state :resolved}] 2000))]
    (is (= ["waiting on 1 artifact(s)"
            "  pending      io.github.test/weave (0s)"
            "  resolved     io.github.test/weave (2s)"]
           (str/split-lines out))
        "the unchanged second poll prints nothing")))

;; =============================================================================
;; Properties
;; =============================================================================

(defspec a-wave-never-returns-before-every-lib-resolves 50
  (prop/for-all [latency (gen/choose 1 5)]
    (let [registry (p/memory-port {:publish-after latency})
          {:keys [now sleep]} (clock)]
      (p/release! registry (step "weave" weave "0.3.1"))
      (p/release! registry (step "dsl" dsl "0.5.9"))
      (let [result (aw/await-wave! registry (assoc directive :timeout-ms 600000)
                                   {:emit (silent) :now now :sleep sleep
                                    :backoff {:start-ms 1000 :cap-ms 1000}})]
        (and (some? (:ok result))
             (every? aw/resolved? (:libs (:ok result))))))))

(defspec a-wave-that-never-publishes-always-times-out 50
  (prop/for-all [timeout (gen/choose 1000 20000)]
    (let [{:keys [now sleep]} (clock)
          result (aw/await-wave! (p/memory-port)
                                 (assoc directive :timeout-ms timeout)
                                 {:emit (silent) :now now :sleep sleep
                                  :backoff {:start-ms 1000 :cap-ms 3000}})]
      (and (nil? (:ok result))
           (= :await/timeout (:kind (:error result)))
           (>= (long (:elapsed-ms (:error result))) (long timeout))))))
