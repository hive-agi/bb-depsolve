(ns bb-depsolve.core.sync-test
  "Tests for internal-dep discovery and the parallel tag-resolution fan-out."
  (:require [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.core.sync :as sync]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [bb-depsolve.core.resolve.registries :as registries]
            [bb-depsolve.version.api :as v]))

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
  (let [clojars-only [{:id "clojars" :url "https://repo.clojars.org" :public? true :version "0.2.21"}]
        reads (fn [versions unread] (fn [& _] {:versions versions :unread unread}))]
    (testing "a Maven-only lib is never asked for a git tag, and keeps its per-registry view"
      (with-redefs [resolve/resolve-lib-tags (fn [& _] (throw (ex-info "must not run" {})))
                    registries/resolve-mvn-reads (reads clojars-only [])]
        (is (= {:ok {:mvn-version "0.2.21" :mvn-by-registry clojars-only}}
               (sync/resolve-sync-lib "." 'io.github.hive-agi/hive-shape
                                      {:dir-name "hive-shape" :coords #{:mvn}})))))
    (testing "the resolver-wide :mvn-version is the newest across registries"
      (with-redefs [registries/resolve-mvn-reads
                    (reads [{:id "clojars" :public? true :version "0.2.21"}
                            {:id "hive-gitea" :public? false :version "0.2.25"}] [])]
        (is (= "0.2.25"
               (:mvn-version (:ok (sync/resolve-sync-lib "." 'io.github.hive-agi/hive-shape
                                                         {:dir-name "hive-shape" :coords #{:mvn}})))))))
    (testing "a registry that did not answer rides along, so consumers can hold their pins"
      (let [unread [{:id "hive-gitea" :url "https://forge.example/m2" :public? false
                     :error :io/unread :status 503}]]
        (with-redefs [registries/resolve-mvn-reads (reads clojars-only unread)]
          (is (= {:ok {:mvn-version "0.2.21" :mvn-by-registry clojars-only :mvn-unread unread}}
                 (sync/resolve-sync-lib "." 'io.github.hive-agi/hive-shape
                                        {:dir-name "hive-shape" :coords #{:mvn}}))))
        (with-redefs [registries/resolve-mvn-reads (reads [] unread)]
          (is (= :io/registry-unread
                 (:error (sync/resolve-sync-lib "." 'io.github.hive-agi/hive-shape
                                                {:dir-name "hive-shape" :coords #{:mvn}})))
              "nothing answered but something failed to: a blind read, not an unpublished lib"))))
    (testing "a mixed lib keeps its tag and its independently published version"
      (with-redefs [resolve/resolve-lib-tags (fn [& _] (r/ok {:tag "v0.1.8" :sha "abcdef0123456789"
                                                              :sha-short "abcdef0" :source :remote}))
                    registries/resolve-mvn-reads (reads [{:id "clojars" :public? true :version "0.1.5"}] [])]
        (is (= {:ok {:tag "v0.1.8" :sha "abcdef0123456789" :sha-short "abcdef0"
                     :source :remote :mvn-version "0.1.5"
                     :mvn-by-registry [{:id "clojars" :public? true :version "0.1.5"}]}}
               (sync/resolve-sync-lib "." 'io.github.hive-agi/hive-schemas
                                      {:dir-name "hive-schemas" :coords #{:git :mvn}})))))
    (testing "a lib whose only coordinate kind is listed nowhere resolves to an error"
      (with-redefs [registries/resolve-mvn-reads (reads [] [])]
        (is (= :io/no-resolved-coordinate
               (:error (sync/resolve-sync-lib "." 'io.github.hive-agi/hive-shape
                                              {:dir-name "hive-shape" :coords #{:mvn}}))))))))

(defn- temp-workspace
  "A throwaway workspace: PROJECTS is {name deps-edn-content}. Returns
   [root-dir dep-files]."
  [projects]
  (let [root (str (java.nio.file.Files/createTempDirectory
                   "depsolve-sync" (into-array java.nio.file.attribute.FileAttribute [])))]
    (doseq [[project content] projects]
      (let [dir (java.io.File. root project)]
        (.mkdirs dir)
        (spit (java.io.File. dir "deps.edn") content)))
    [root (mapv (fn [[project _]]
                  {:path (str root "/" project "/deps.edn") :type :deps-edn :project project})
                projects)]))

(deftest compute-sync-changes-pins-each-consumer-to-what-it-can-reach-test
  (let [gitea "https://forge.example/api/packages/acme/maven"
        by-registry [{:id "clojars" :url "https://repo.clojars.org" :public? true :version "0.1.0"}
                     {:id "hive-gitea" :url gitea :public? false :version "0.1.1"}]
        resolved {'io.github.hive-agi/hive-help {:mvn-version "0.1.1" :mvn-by-registry by-registry}}
        pin (fn [v] (str "{:deps {io.github.hive-agi/hive-help {:mvn/version \"" v "\"}}}"))
        [_ dep-files] (temp-workspace
                       {"public-behind" (pin "0.0.9")
                        "public-current" (pin "0.1.0")
                        "public-overshot" (pin "0.1.1")
                        "private" (str "{:mvn/repos {\"hive-gitea\" {:url \"" gitea "/\"}} "
                                       ":deps {io.github.hive-agi/hive-help {:mvn/version \"0.1.0\"}}}")})
        changes (sync/compute-sync-changes dep-files resolved)
        by-project (into {} (map (juxt :project identity)) changes)]
    (is (= #{"public-behind" "public-overshot" "private"} (set (keys by-project)))
        "a public project already at the newest PUBLIC version has nothing to do")
    (is (= {:old-version "0.0.9" :new-version "0.1.0" :source "clojars"}
           (select-keys (by-project "public-behind") [:old-version :new-version :source]))
        "a public project rises only to what Clojars has")
    (is (= [{:id "hive-gitea" :url gitea :public? false :version "0.1.1"}]
           (:unreachable (by-project "public-behind")))
        "the newer private version is named, not written")
    (is (= {:old-version "0.1.1" :new-version "0.1.0" :source "clojars"}
           (select-keys (by-project "public-overshot") [:old-version :new-version :source]))
        "a public project pinned past Clojars is brought back to what it can fetch")
    (is (= {:old-version "0.1.0" :new-version "0.1.1" :source "hive-gitea"}
           (select-keys (by-project "private") [:old-version :new-version :source]))
        "a project declaring the private registry (trailing slash and all) reaches its newest")
    (is (nil? (:unreachable (by-project "private"))))))

(deftest compute-withheld-names-pins-sync-will-not-move-test
  (let [gitea "https://forge.example/api/packages/acme/maven"
        private-only [{:id "hive-gitea" :url gitea :public? false :version "0.2.5"}]
        clojars-unread [{:id "clojars" :url "https://repo.clojars.org" :public? true
                         :error :io/unread :status 503}]
        resolved {'io.github.hive-agi/hive-contracts {:mvn-version "0.2.5" :mvn-by-registry private-only}
                  'io.github.hive-agi/hive-dsl {:mvn-version "0.5.9"
                                                :mvn-by-registry [{:id "hive-gitea" :url gitea :public? false :version "0.5.9"}]
                                                :mvn-unread clojars-unread}}
        [_ dep-files] (temp-workspace
                       {"forgot-registry" "{:deps {io.github.hive-agi/hive-contracts {:mvn/version \"0.2.0\"}}}"
                        "declares-it" (str "{:mvn/repos {\"hive-gitea\" {:url \"" gitea "\"}} "
                                           ":deps {io.github.hive-agi/hive-contracts {:mvn/version \"0.2.0\"}"
                                           "       io.github.hive-agi/hive-dsl {:mvn/version \"0.5.21\"}}}")
                        "does-not-pin-it" "{:deps {org.clojure/clojure {:mvn/version \"1.12.0\"}}}"})]
    (is (= [{:project "declares-it" :lib 'io.github.hive-agi/hive-dsl :reason :unread :unread clojars-unread}
            {:project "forgot-registry" :lib 'io.github.hive-agi/hive-contracts :reason :unreachable :versions private-only}]
           (->> (sync/compute-withheld dep-files resolved)
                (map #(dissoc % :path))
                (sort-by :project)))
        "the project that pins without declaring the registry, and the project whose
         reachable registry did not answer, are both reported with their reason")
    (is (= [['io.github.hive-agi/hive-contracts "declares-it"]]
           (mapv (juxt :lib :project) (sync/compute-sync-changes dep-files resolved)))
        "sync moves only the pin it can certify: hive-dsl is NOT rolled back to the
         private 0.5.9 while Clojars is unread")))

(deftest a-downgrade-row-is-recognised-test
  (is (v/downgrade-change? {:coord :mvn :old-version "0.4.0" :new-version "0.3.11"}))
  (is (not (v/downgrade-change? {:coord :mvn :old-version "0.3.11" :new-version "0.4.0"})))
  (is (not (v/downgrade-change? {:coord :mvn :old-version "0.4.0" :new-version "0.4.0"})))
  (is (v/downgrade-change? {:coord :git :old-tag "v0.5.21" :new-tag "v0.5.9"}))
  (is (not (v/downgrade-change? {:coord :git :old-tag "v0.5.9" :new-tag "v0.5.21"})))
  (is (not (v/downgrade-change? {:coord :other})) "total"))

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
