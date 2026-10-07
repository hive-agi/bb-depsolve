(ns bb-depsolve.harmony.rules-trifecta-test
  "Golden + property + mutation coverage for the harmony rule chain.

   The mutation that matters is the ORDERING one: a chain that asks about
   findings before asking whether anything was read reports a blind check as
   agreement, which is the exact failure this subsystem exists to avoid."
  (:require [bb-depsolve.harmony.rules :as rules]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

(defn- ctx
  [verified? aot-jars finding-count]
  {:verified? verified? :aot-jars aot-jars :finding-count finding-count})

(def ^:private gen-ctx
  (gen/let [verified? gen/boolean
            aot (gen/choose 0 20)
            findings (gen/choose 0 20)]
    (ctx verified? aot findings)))

(defn- declared-verdict?
  [v]
  (contains? rules/verdicts v))

(tri/deftrifecta harmony-classify-trifecta
  bb-depsolve.harmony.rules/classify
  {:golden-path "test/golden/bb-depsolve/harmony-classify.edn"
   :cases {:clean               (ctx true 12 0)
           :broken              (ctx true 12 1)
           :nothing-aot-at-all  (ctx true 0 0)
           :blind               (ctx false 0 0)
           :blind-with-findings (ctx false 3 2)}
   :gen gen-ctx
   :pred declared-verdict?
   :num-tests 300
   :mutations [["asks-about-findings-before-asking-if-anything-was-read"
                (fn [{:keys [verified? finding-count]}]
                  (cond (pos? (long finding-count)) :broken
                        (not verified?) :unverified
                        :else :agreed))]
               ["treats-no-aot-jars-as-unverified"
                (fn [{:keys [verified? aot-jars finding-count]}]
                  (cond (not verified?) :unverified
                        (zero? (long aot-jars)) :unverified
                        (pos? (long finding-count)) :broken
                        :else :agreed))]]})

(deftest ordering-is-the-contract
  (testing "a blind check with findings is unverified, not broken"
    (is (= :unverified (rules/classify (ctx false 3 2)))
        "findings from an unread classpath are not evidence of anything"))

  (testing "a verified check with no AOT jars agrees"
    (is (= :agreed (rules/classify (ctx true 0 0)))
        "nothing to disagree with is agreement, not a blind spot")))

(deftest chain-is-open-for-extension
  (testing "a prepended rule decides before every built-in one"
    (let [rules (cons {:name :everything-is-fine-here
                       :when (constantly true)
                       :verdict :agreed}
                      rules/default-harmony-rules)]
      (is (= :agreed (rules/classify rules (ctx false 3 9)))
          "extension is prepending a rule, never editing the fold")))

  (testing "a verdict may be a function of the context"
    (let [rules [{:name :echo :when (constantly true)
                  :verdict (fn [c] (if (:verified? c) :agreed :unverified))}]]
      (is (= :agreed (rules/classify rules (ctx true 1 0))))
      (is (= :unverified (rules/classify rules (ctx false 1 0))))))

  (testing "an empty chain decides nothing rather than guessing"
    (is (nil? (rules/classify [] (ctx true 1 1))))))
