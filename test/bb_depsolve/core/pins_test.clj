(ns bb-depsolve.core.pins-test
  "The pins file: what it holds, whose pin wins, and when a pin has gone stale."
  (:require [bb-depsolve.core.pins :as pins]
            [bb-depsolve.core.upgrade.guard :as guard]
            [clojure.test :refer [deftest is testing]]))

(defn- pins-of [& entries]
  (pins/parse {:pins (vec entries)}))

;; =============================================================================
;; Reading the file
;; =============================================================================

(deftest a-bare-lib-name-stands-for-group-equals-artifact-test
  (is (= 'cheshire/cheshire (pins/normalize-lib 'cheshire)))
  (is (= 'org.clojure/core.cache (pins/normalize-lib 'org.clojure/core.cache))
      "an already-qualified lib is left alone")
  (is (nil? (pins/normalize-lib nil))))

(deftest an-entry-naming-no-lib-is-dropped-rather-than-held-test
  (let [p (pins-of {:reason "forgot the lib"} {:lib 'cheshire})]
    (is (= '[cheshire/cheshire] (mapv :lib (:entries p)))
        "a pin with no :lib holds nothing, so it is not an entry")))

(deftest the-last-entry-for-a-lib-wins-so-the-file-reads-top-to-bottom-test
  (let [p (pins-of {:lib 'cheshire/cheshire :version "5.0.0" :reason "first"}
                   {:lib 'cheshire/cheshire :version "6.0.0" :reason "second"})]
    (is (= "6.0.0" (:version (pins/pin-for p 'cheshire/cheshire nil))))
    (is (= "second" (:reason (pins/pin-for p 'cheshire/cheshire nil))))))

;; =============================================================================
;; Whose pin applies
;; =============================================================================

(deftest a-project-scoped-pin-beats-a-workspace-wide-one-test
  (let [p (pins-of {:lib 'org.clojure/core.cache :version "1.0.0" :reason "everywhere"}
                   {:lib 'org.clojure/core.cache :version "1.2.263"
                    :project "hive-cache" :reason "just here"})]
    (is (= "1.2.263" (:version (pins/pin-for p 'org.clojure/core.cache "hive-cache")))
        "the narrower pin is the one that decides")
    (is (= "1.0.0" (:version (pins/pin-for p 'org.clojure/core.cache "hive-mcp")))
        "every other project still sees the workspace-wide pin")))

(deftest a-pin-covers-nothing-it-does-not-name-test
  (let [p (pins-of {:lib 'org.clojure/core.cache :project "hive-cache"})]
    (is (pins/pinned? p 'org.clojure/core.cache "hive-cache"))
    (is (not (pins/pinned? p 'org.clojure/core.cache "hive-mcp"))
        "a project-scoped pin does not leak into its neighbours")
    (is (not (pins/pinned? p 'cheshire/cheshire "hive-cache")))
    (is (not (pins/pinned? nil 'org.clojure/core.cache "hive-cache"))
        "no pins file at all holds nothing")))

;; =============================================================================
;; Auditing a pin against what the workspace declares
;; =============================================================================

(def ^:private deps
  [{:lib 'org.clojure/core.cache :project "hive-cache" :version "1.2.263"}
   {:lib 'org.clojure/core.cache :project "hive-mcp"   :version "1.1.234"}])

(deftest a-pin-the-files-agree-with-is-held-test
  (let [rows (pins/audit (pins-of {:lib 'org.clojure/core.cache
                                   :project "hive-cache"
                                   :version "1.2.263"})
                         deps)]
    (is (= [:held] (mapv :status rows)))
    (is (= "hive-cache" (:project (first rows))))))

(deftest a-pin-the-file-moved-underneath-is-drifted-test
  (let [rows (pins/audit (pins-of {:lib 'org.clojure/core.cache
                                   :project "hive-mcp"
                                   :version "1.2.263"})
                         deps)]
    (is (= [:drifted] (mapv :status rows)))
    (is (= "1.1.234" (:actual-version (first rows)))
        "the row names what the file actually says, not only that it disagrees")))

(deftest a-pin-with-no-version-forbids-movement-without-asserting-a-place-test
  (let [rows (pins/audit (pins-of {:lib 'org.clojure/core.cache :reason "no version"})
                         deps)]
    (is (= [:held :held] (mapv :status rows))
        "a pin that records no version can never drift")))

(deftest a-pin-naming-a-lib-nobody-declares-is-unused-test
  (let [rows (pins/audit (pins-of {:lib 'cheshire/cheshire :version "5.0.0"}) deps)]
    (is (= [:unused] (mapv :status rows)))
    (is (nil? (:actual-version (first rows))))))

(deftest a-workspace-wide-pin-is-audited-once-per-project-test
  (let [rows (pins/audit (pins-of {:lib 'org.clojure/core.cache :version "1.2.263"}) deps)]
    (is (= 2 (count rows)) "one row per dep the pin covers")
    (is (= #{"hive-cache" "hive-mcp"} (set (map :project rows))))
    (is (= {:held 1 :drifted 1} (pins/status-counts rows))
        "the same pin can be honoured in one project and stale in another")))

;; =============================================================================
;; What the guard does with a pin
;; =============================================================================

(def ^:private dep
  {:path "/w/hive-cache/deps.edn" :project "hive-cache"
   :lib 'org.clojure/core.cache :version "1.2.263" :consumer-repos []})

(def ^:private latest
  {'org.clojure/core.cache [{:id "central" :version "1.2.999" :public? true}]})

(deftest an-unpinned-lib-is-offered-as-usual-test
  (let [{:keys [upgrades held]} (guard/plan [dep] latest {})]
    (is (= ["1.2.999"] (mapv :new-version upgrades)))
    (is (empty? held))))

(deftest a-pinned-lib-is-held-and-says-so-test
  (let [p (pins-of {:lib 'org.clojure/core.cache :project "hive-cache"
                    :version "1.2.263" :reason "single-flight test flakes above this"})
        {:keys [upgrades held]} (guard/plan [dep] latest {:pins p})]
    (is (empty? upgrades) "a pinned lib is never written")
    (is (= [:pinned] (mapv :reason held)))
    (is (= "single-flight test flakes above this" (get-in (first held) [:pin :reason]))
        "the reason rides along, so the report can say why it will not move")))

(deftest a-pin-is-reported-instead-of-vanishing-the-way-exclude-does-test
  (let [p (pins-of {:lib 'org.clojure/core.cache :project "hive-cache"})
        {:keys [held]} (guard/plan [dep] latest {:pins p})
        {:keys [upgrades]} (guard/plan [dep] latest {:exclude "org.clojure/core.cache"})]
    (is (= 1 (count held))
        "a pin still produces a row, which is the whole point of recording it")
    (is (= ["1.2.999"] (mapv :new-version upgrades))
        "--exclude is a lib SELECTION flag: guard/plan never sees it, so the row survives")))

(deftest a-pin-beats-the-reason-the-version-policy-would-have-given-test
  (let [major-dep (assoc dep :version "0.2.0")
        major-latest {'org.clojure/core.cache [{:id "central" :version "0.4.0" :public? true}]}]
    (testing "without a pin the jump is held as a pre-1.0 minor"
      (is (= [:major] (mapv :reason (:held (guard/plan [major-dep] major-latest {}))))))
    (testing "with a pin the operator's recorded decision is what gets reported"
      (let [p (pins-of {:lib 'org.clojure/core.cache :reason "waiting on the 1.0 API"})]
        (is (= [:pinned] (mapv :reason (:held (guard/plan [major-dep] major-latest {:pins p})))))))))
