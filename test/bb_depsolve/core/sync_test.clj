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
  {'io.github.hive-agi/hive-dsl   {:dir-name "hive-dsl" :coords #{:git}}
   'io.github.hive-agi/hive-test  {:dir-name "hive-test" :coords #{:git}}
   'io.github.hive-agi/hive-weave {:dir-name "hive-weave" :coords #{:git}}})

(defn- resolved-as [tag]
  {:tag tag :sha "abcdef1234" :sha-short "abcdef1" :source :remote})

;; =============================================================================
;; Unit — discover-internal-libs
;; =============================================================================

(deftest discover-internal-libs-preserves-coordinate-kinds-test
  (let [dep-files [{:path "/w/a/deps.edn" :type :deps-edn :project "a"}]
        content (str "{:deps {io.github.hive-agi/hive-shape {:mvn/version \"0.2.20\"}\n"
                     "        io.github.hive-agi/hive-test {:git/tag \"v0.3.7\" :git/sha \"abcdef0\"\n"
                     "                                      :mvn/version \"0.3.7\"}\n"
                     "        outsider/lib {:mvn/version \"1.0.0\"}}}")]
    (with-redefs [slurp (constantly content)]
      (is (= {'io.github.hive-agi/hive-shape {:dir-name "hive-shape" :coords #{:mvn}}
              'io.github.hive-agi/hive-test {:dir-name "hive-test" :coords #{:git :mvn}}}
             (sync/discover-internal-libs dep-files "hive-agi"))
          "only io.github.hive-agi/* coords are internal, and each lib remembers
           which coordinate kinds must be resolved for it"))))

(deftest resolve-sync-lib-uses-the-authoritative-source-per-coordinate-test
  (testing "a Maven-only lib is never asked for a git tag"
    (with-redefs [resolve/resolve-lib-tags (fn [& _] (throw (ex-info "must not run" {})))
                  resolve/resolve-mvn-latest (fn [& _] (r/ok "0.2.21"))]
      (is (= {:ok {:mvn-version "0.2.21"}}
             (sync/resolve-sync-lib "." 'io.github.hive-agi/hive-shape
                                    {:dir-name "hive-shape" :coords #{:mvn}})))))
  (testing "a mixed lib keeps its tag and its independently published version"
    (with-redefs [resolve/resolve-lib-tags (fn [& _] (r/ok {:tag "v0.1.8" :sha "abcdef0123456789"
                                                            :sha-short "abcdef0" :source :remote}))
                  resolve/resolve-mvn-latest (fn [& _] (r/ok "0.1.5"))]
      (is (= {:ok {:tag "v0.1.8" :sha "abcdef0123456789" :sha-short "abcdef0"
                   :source :remote :mvn-version "0.1.5"}}
             (sync/resolve-sync-lib "." 'io.github.hive-agi/hive-schemas
                                    {:dir-name "hive-schemas" :coords #{:git :mvn}})))))
  (testing "a lib whose only coordinate kind fails resolves to an error"
    (with-redefs [resolve/resolve-mvn-latest (fn [& _] (r/err :io/no-published-version))]
      (is (= :io/no-resolved-coordinate
             (:error (sync/resolve-sync-lib "." 'io.github.hive-agi/hive-shape
                                            {:dir-name "hive-shape" :coords #{:mvn}})))))))

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
  (testing "a lib whose coordinates all fail is reported, not silently dropped"
    (with-redefs [resolve/resolve-lib-tags
                  (fn [_ lib _]
                    (if (= 'io.github.hive-agi/hive-test lib)
                      (r/err :parse/no-semver-tags {:lib lib})
                      (r/ok (resolved-as "v1.0.0"))))]
      (let [{:keys [resolved failed]} (sync/resolve-internal-libs "/w" libs)]
        (is (= 2 (count resolved)))
        (is (= [{:lib 'io.github.hive-agi/hive-test :error :io/no-resolved-coordinate}]
               failed)))))
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
