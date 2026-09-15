(ns bb-depsolve.core.upgrade.guard-test
  "The upgrade guard's lib selection, major hold, per-consumer projection and
   downgrade refusal, as pure values."
  (:require [bb-depsolve.core.upgrade.guard :as guard]
            [clojure.test :refer [deftest is testing]]))

(def ^:private clojars {:id "clojars" :url "https://repo.clojars.org/" :public? true})
(def ^:private gitea   {:id "gitea" :url "https://git.example.com/api/packages/acme/maven" :public? false})
(def ^:private declares-gitea [{:id "gitea" :url "https://git.example.com/api/packages/acme/maven"}])

;; =============================================================================
;; Lib selection
;; =============================================================================

(deftest parse-libs-reads-a-csv-of-qualified-names-test
  (is (= #{'cheshire/cheshire 'org.apache.tika/tika-core}
         (guard/parse-libs " cheshire/cheshire , org.apache.tika/tika-core,")))
  (is (= #{'cheshire/cheshire} (guard/parse-libs "cheshire"))
      "a bare name stands for group = artifact")
  (is (nil? (guard/parse-libs nil)))
  (is (nil? (guard/parse-libs "  ")))
  (is (nil? (guard/parse-libs ",,")) "a csv with no entries names nothing"))

(deftest partition-libs-excludes-the-internal-org-test
  (let [libs '[io.github.hive-agi/hive-dsl cheshire/cheshire io.github.other/x]]
    (is (= {:kept '[cheshire/cheshire io.github.other/x]
            :internal '[io.github.hive-agi/hive-dsl]
            :excluded []}
           (guard/partition-libs libs {}))
        "the default org is hive-agi")
    (is (= '[io.github.other/x] (:internal (guard/partition-libs libs {:org "other"})))
        "--org names the group sync owns")))

(deftest partition-libs-applies-only-and-exclude-test
  (let [libs '[cheshire/cheshire org.apache.tika/tika-core metosin/malli
               io.github.hive-agi/hive-dsl]]
    (testing "--only keeps just the named libs"
      (is (= {:kept '[cheshire/cheshire]
              :internal '[io.github.hive-agi/hive-dsl]
              :excluded '[metosin/malli org.apache.tika/tika-core]}
             (guard/partition-libs libs {:only "cheshire/cheshire"}))))
    (testing "--exclude drops the named libs"
      (is (= '[cheshire/cheshire metosin/malli]
             (:kept (guard/partition-libs libs {:exclude "org.apache.tika/tika-core"})))))
    (testing "an internal lib is not released by naming it in --only"
      (is (= [] (:kept (guard/partition-libs libs {:only "io.github.hive-agi/hive-dsl"})))))))

(deftest apply-mode-test
  (is (= :interactive (guard/apply-mode {})))
  (is (= :explicit (guard/apply-mode {:all true})))
  (is (= :explicit (guard/apply-mode {:only "cheshire/cheshire"})))
  (is (= :interactive (guard/apply-mode {:only " "})) "a blank --only selects nothing"))

;; =============================================================================
;; Version policy
;; =============================================================================

(deftest held-jump?-test
  (testing "a major increase is held"
    (is (guard/held-jump? "3.3.2" "4.0.0"))
    (is (guard/held-jump? "0.9.1" "1.0.0")))
  (testing "a minor increase under 0.x is held"
    (is (guard/held-jump? "0.2.318" "0.4.1"))
    (is (guard/held-jump? "0.2.318" "0.3.0")))
  (testing "patch moves and minor moves past 1.0 are not"
    (is (not (guard/held-jump? "0.2.318" "0.2.800")))
    (is (not (guard/held-jump? "1.2.0" "1.9.0")))
    (is (not (guard/held-jump? "3.3.2" "3.3.3"))))
  (testing "total on odd input"
    (is (not (guard/held-jump? "4.0.0" "3.3.2")) "a downgrade is not a jump")
    (is (not (guard/held-jump? "20231013" "20240303")) "calendar builds are not held")
    (is (not (guard/held-jump? nil "1.0.0")))
    (is (not (guard/held-jump? "1.0.0" nil)))))

(deftest consumer-candidate-projects-through-declared-registries-test
  (let [rows [(assoc clojars :version "0.1.0") (assoc gitea :version "0.1.1")]]
    (testing "a public-only consumer sees only the public version"
      (is (= {:version "0.1.0" :unreachable [(assoc gitea :version "0.1.1")]}
             (guard/consumer-candidate rows []))))
    (testing "a consumer declaring the private registry sees its version"
      (is (= {:version "0.1.1" :unreachable []}
             (guard/consumer-candidate rows declares-gitea))))
    (testing "nothing reachable"
      (is (= {:version nil :unreachable [(assoc gitea :version "0.1.1")]}
             (guard/consumer-candidate [(assoc gitea :version "0.1.1")] []))))))

(def ^:private dep {:path "/w/alpha/deps.edn" :project "alpha" :lib 'acme/lib})

(deftest decide-test
  (testing "a plain upgrade"
    (is (= (assoc dep :old-version "1.0.0" :new-version "1.2.0")
           (guard/decide (assoc dep :version "1.0.0" :consumer-repos [])
                         {:version "1.2.0" :unreachable []} {}))))
  (testing "a major is held unless allowed"
    (is (= :major (:reason (guard/decide (assoc dep :version "3.3.2")
                                         {:version "4.0.0" :unreachable []} {}))))
    (is (nil? (:reason (guard/decide (assoc dep :version "3.3.2")
                                     {:version "4.0.0" :unreachable []} {:allow-major true})))))
  (testing "a newest-reachable version below the pin is a refused downgrade"
    (is (= (assoc dep :old-version "0.1.1" :new-version "0.1.0" :reason :downgrade)
           (guard/decide (assoc dep :version "0.1.1")
                         {:version "0.1.0" :unreachable [(assoc gitea :version "0.1.1")]}
                         {:allow-major true}))
        "--allow-major never releases a downgrade"))
  (testing "a newer version only on an undeclared registry is held, named"
    (is (= (assoc dep :old-version "0.1.0" :new-version "0.1.1"
                  :reason :unreachable :registry "gitea")
           (guard/decide (assoc dep :version "0.1.0")
                         {:version "0.1.0" :unreachable [(assoc gitea :version "0.1.1")]} {}))))
  (testing "nothing to move"
    (is (nil? (guard/decide (assoc dep :version "1.2.0") {:version "1.2.0" :unreachable []} {})))
    (is (nil? (guard/decide (assoc dep :version "1.2.0") {:version nil :unreachable []} {})))))

(deftest plan-scopes-each-consumer-to-its-registries-test
  (let [latest {'acme/lib [(assoc clojars :version "1.1.0") (assoc gitea :version "1.2.0")]
                'big/lib  [(assoc clojars :version "4.0.0")]}
        deps [{:path "/w/pub/deps.edn" :project "pub" :lib 'acme/lib :version "1.0.0" :consumer-repos []}
              {:path "/w/priv/deps.edn" :project "priv" :lib 'acme/lib :version "1.0.0"
               :consumer-repos declares-gitea}
              {:path "/w/pub/deps.edn" :project "pub" :lib 'big/lib :version "3.3.2" :consumer-repos []}
              {:path "/w/pub/deps.edn" :project "pub" :lib 'gone/lib :version "1.0.0" :consumer-repos []}]
        {:keys [upgrades held]} (guard/plan deps latest {})]
    (is (= [{:path "/w/pub/deps.edn" :project "pub" :lib 'acme/lib :old-version "1.0.0" :new-version "1.1.0"}
            {:path "/w/priv/deps.edn" :project "priv" :lib 'acme/lib :old-version "1.0.0" :new-version "1.2.0"}]
           upgrades)
        "each consumer moves to the newest version its own registries serve")
    (is (= [{:path "/w/pub/deps.edn" :project "pub" :lib 'big/lib :old-version "3.3.2"
             :new-version "4.0.0" :reason :major}]
           held))
    (is (= 3 (count (:upgrades (guard/plan deps latest {:allow-major true}))))
        "--allow-major releases the held major")))
