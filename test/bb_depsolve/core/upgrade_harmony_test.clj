(ns bb-depsolve.core.upgrade-harmony-test
  "The re-partition that turns harmony findings into held rows.

   Pure: `hold-disharmonious` takes an already-made plan and an already-made
   findings map, which is what keeps the expensive classpath read out of
   `plan` and out of this test."
  (:require [bb-depsolve.core.upgrade.guard :as guard]
            [clojure.test :refer [deftest is testing]]))

(def ^:private row-a
  {:project "alpha" :path "/w/alpha/deps.edn" :lib 'org.clojure/clojure
   :old-version "1.12.5" :new-version "1.12.6"})

(def ^:private row-b
  {:project "beta" :path "/w/beta/deps.edn" :lib 'org.clojure/clojure
   :old-version "1.12.5" :new-version "1.12.6"})

(def ^:private finding
  {:verdict :broken
   :findings [{:jar "/m2/deep-diamond-0.47.1.jar"
               :missing-count 19
               :missing ["clojure/core$seq_QMARK___5492"]}]})

(deftest holds-exactly-the-rows-with-evidence
  (let [planned {:upgrades [row-a row-b] :held []}
        result (guard/hold-disharmonious planned {row-a finding})]
    (testing "the row with a finding moves to held, carrying it"
      (is (= [(assoc row-a :reason :disharmony :harmony finding)] (:held result))))

    (testing "the same lib in another project is untouched"
      (is (= [row-b] (:upgrades result))
          "harmony is a property of a classpath, so the verdict is per project"))))

(deftest keeps-the-rows-other-guards-already-held
  (let [pinned (assoc row-b :reason :pinned)
        result (guard/hold-disharmonious {:upgrades [row-a] :held [pinned]}
                                         {row-a finding})]
    (is (= 2 (count (:held result))))
    (is (some #(= :pinned (:reason %)) (:held result))
        "an earlier reason is never overwritten by this pass")))

(deftest no-findings-is-a-no-op
  (let [planned {:upgrades [row-a row-b] :held []}]
    (is (= planned (guard/hold-disharmonious planned {}))
        "a workspace with nothing AOT on it pays nothing and changes nothing")))
