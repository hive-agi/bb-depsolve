(ns bb-depsolve.schema-synth-test
  "Schema-synthesized coverage: the registered malli schema supplies the
   generator, the oracle and the corruptions, so these tests need no
   hand-written fixtures. See hive-schemas.test.

   Every predicate here guards a contract the sync path depends on, so a
   schema that drifts from its predicate fails here rather than in a release."
  (:require [bb-depsolve.schema :as sch]
            [clojure.test :refer [deftest is]]
            [hive-schemas.schema :as hs]
            [hive-schemas.test :as hst]))

(sch/register!)

;; =============================================================================
;; Predicates under test — each is the contract, stated once
;; =============================================================================

(defn resolved-lib?
  "A resolution carries a git coordinate, a Maven coordinate, or both."
  [x]
  (hs/validate :bb-depsolve/resolved-lib x))

(defn sync-change?
  "A sync change names the coordinate kind it rewrites and both its ends."
  [x]
  (hs/validate :bb-depsolve/sync-change x))

(defn cascade-step?
  [x]
  (hs/validate :bb-depsolve/cascade-step x))

;; =============================================================================
;; Synthesized trifectas
;; =============================================================================

;; :bb-depsolve/resolved-lib gets NO synthesized predicate trifecta. Every one
;; of its entries is optional — a git-only and a Maven-only resolution are both
;; complete — so the synthesizer can derive no corruption to reject, and it says
;; so rather than passing vacuously. Its real constraint is the :fn below the
;; map, which is covered explicitly at the bottom of this namespace.

(hst/deftrifecta-predicate sync-change-contract
  bb-depsolve.schema-synth-test/sync-change?
  {:schema :bb-depsolve/sync-change})

(hst/deftrifecta-predicate cascade-step-contract
  bb-depsolve.schema-synth-test/cascade-step?
  {:schema :bb-depsolve/cascade-step})

;; =============================================================================
;; The one property the generator cannot state for us
;; =============================================================================

(deftest a-resolution-must-carry-at-least-one-coordinate-test
  (is (not (resolved-lib? {}))
      "an empty map resolved nothing and must not pass as a resolution")
  (is (not (resolved-lib? {:tag "v0.1.0"}))
      "a tag without its sha is half a git coordinate")
  (is (resolved-lib? {:mvn-version "0.1.0"})
      "a Maven-only lib never has a tag — that is a complete resolution"))
