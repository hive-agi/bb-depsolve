(ns bb-depsolve.layer.rules-trifecta-test
  "Golden + property + mutation coverage for the layer rule chain.

   The mutation facet pins the two regressions that would make the guard
   useless: a chain that forgets terminal layers, and one that reads the
   level comparison backwards."
  (:require [bb-depsolve.layer.rules :as rules]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- ctx
  [from-level to-level & {:keys [terminal? waived?]}]
  {:from "a" :to "b"
   :from-level from-level :to-level to-level
   :to-terminal? (boolean terminal?)
   :waived? (boolean waived?)})

(def ^:private gen-ctx
  "Contexts spanning every branch: ranked and unranked ends, terminal targets,
   waivers, and all three level orderings."
  (gen/let [from (gen/one-of [(gen/return nil) (gen/choose 0 5)])
            to   (gen/one-of [(gen/return nil) (gen/choose 0 5)])
            term gen/boolean
            waiv gen/boolean]
    {:from "a" :to "b"
     :from-level from :to-level to
     :to-terminal? term :waived? waiv}))

(defn- declared-verdict?
  [v]
  (contains? rules/verdicts v))

;; =============================================================================
;; Trifecta
;; =============================================================================

(tri/deftrifecta layer-classify-trifecta
  bb-depsolve.layer.rules/classify
  {:golden-path "test/golden/bb-depsolve/layer-classify.edn"
   :cases       {:downward          (ctx 3 1)
                 :sideways          (ctx 2 2)
                 :upward            (ctx 1 3)
                 :terminal-target   (ctx 2 5 :terminal? true)
                 :terminal-sideways (ctx 5 5 :terminal? true)
                 :waiver-beats-terminal (ctx 2 5 :terminal? true :waived? true)
                 :unranked-consumer (ctx nil 2)
                 :unranked-dep      (ctx 2 nil)
                 :unranked-beats-waiver (ctx nil 5 :terminal? true :waived? true)}
   :gen         gen-ctx
   :pred        declared-verdict?
   :num-tests   300
   :mutations   [["ignores-terminal-layers"
                  (fn [{:keys [from-level to-level waived?]}]
                    (cond (or (nil? from-level) (nil? to-level)) :unranked
                          waived?                                :waived
                          (> (long from-level) (long to-level))  :ok
                          (= from-level to-level)                :sideways
                          :else                                  :violation))]
                 ["reads-the-level-comparison-backwards"
                  (fn [{:keys [from-level to-level to-terminal? waived?]}]
                    (cond (or (nil? from-level) (nil? to-level)) :unranked
                          waived?                                :waived
                          to-terminal?                           :violation
                          (< (long from-level) (long to-level))  :ok
                          (= from-level to-level)                :sideways
                          :else                                  :violation))]
                 ["lets-a-waiver-override-an-unranked-end"
                  (fn [{:keys [from-level to-level to-terminal? waived?]}]
                    (cond waived?                                :waived
                          (or (nil? from-level) (nil? to-level)) :unranked
                          to-terminal?                           :violation
                          (> (long from-level) (long to-level))  :ok
                          (= from-level to-level)                :sideways
                          :else                                  :violation))]]})
