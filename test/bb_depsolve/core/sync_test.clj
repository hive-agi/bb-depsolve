(ns bb-depsolve.core.sync-test
  "Tests for internal-dep discovery and the parallel tag-resolution fan-out."
  (:require [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.core.sync :as sync]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private libs
  {'io.github.hive-agi/hive-dsl "hive-dsl"
   'io.github.hive-agi/hive-test "hive-test"
   'io.github.hive-agi/hive-weave "hive-weave"})

(defn- resolved-as [tag]
  {:tag tag :sha "abcdef1234" :sha-short "abcdef1" :source :remote})

;; =============================================================================
;; Unit — discover-internal-libs
;; =============================================================================

(deftest discover-internal-libs-finds-git-and-mvn-coords-test
  (let [dep-files [{:path "/w/a/deps.edn" :type :deps-edn :project "a"}]
        content (str "{:deps {io.github.hive-agi/hive-dsl {:git/tag \"v0.5.8\" :git/sha \"755baf5\"}\n"
                     "        io.github.hive-agi/hive-weave {:mvn/version \"0.3.1\"}\n"
                     "        metosin/malli {:mvn/version \"0.20.1\"}}}")]
    (with-redefs [slurp (constantly content)]
      (let [found (sync/discover-internal-libs dep-files "hive-agi")]
        (is (= #{'io.github.hive-agi/hive-dsl 'io.github.hive-agi/hive-weave}
               (set (keys found)))
            "only io.github.hive-agi/* coords are internal, in either coord shape")
        (is (= "hive-dsl" (found 'io.github.hive-agi/hive-dsl))
            "the value is the sibling directory name")))))

;; =============================================================================
;; Unit — resolve-internal-libs (bounded fan-out)
;; =============================================================================

(deftest resolve-internal-libs-resolves-every-lib-test
  (with-redefs [resolve/resolve-lib-tags (fn [_ lib _] (r/ok (resolved-as (str "v-" (name lib)))))]
    (let [{:keys [resolved failed]} (sync/resolve-internal-libs "/w" libs)]
      (is (= [] failed))
      (is (= (set (keys libs)) (set (keys resolved))))
      (is (= "v-hive-weave" (:tag (resolved 'io.github.hive-agi/hive-weave)))
          "results are keyed back to the lib that produced them, not by position"))))

(deftest resolve-internal-libs-reports-what-it-could-not-resolve-test
  (testing "an error Result keeps its own error key"
    (with-redefs [resolve/resolve-lib-tags
                  (fn [_ lib _]
                    (if (= 'io.github.hive-agi/hive-test lib)
                      (r/err :parse/no-semver-tags {:lib lib})
                      (r/ok (resolved-as "v1.0.0"))))]
      (let [{:keys [resolved failed]} (sync/resolve-internal-libs "/w" libs)]
        (is (= 2 (count resolved)))
        (is (= [{:lib 'io.github.hive-agi/hive-test :error :parse/no-semver-tags}] failed)
            "an unresolvable lib is reported, not silently dropped"))))
  (testing "a thrown lookup becomes :io/resolve-failed"
    (with-redefs [resolve/resolve-lib-tags
                  (fn [_ lib _]
                    (if (= 'io.github.hive-agi/hive-dsl lib)
                      (throw (ex-info "ls-remote exploded" {}))
                      (r/ok (resolved-as "v1.0.0"))))]
      (let [{:keys [resolved failed]} (sync/resolve-internal-libs "/w" libs)]
        (is (= 2 (count resolved)))
        (is (= [{:lib 'io.github.hive-agi/hive-dsl :error :io/resolve-failed}] failed))))))

(deftest resolve-internal-libs-handles-an-empty-workspace-test
  (is (= {:resolved {} :failed []} (sync/resolve-internal-libs "/w" {}))))

(deftest resolve-internal-libs-runs-concurrently-test
  (let [running (atom 0)
        peak (atom 0)]
    (with-redefs [resolve/resolve-lib-tags
                  (fn [_ _ _]
                    (let [n (swap! running inc)]
                      (swap! peak max n)
                      (Thread/sleep 120)
                      (swap! running dec)
                      (r/ok (resolved-as "v1.0.0"))))]
      (sync/resolve-internal-libs "/w" libs)
      (is (> @peak 1)
          "tag lookups overlap — sync's wall clock is remote latency, not its sum"))))
