(ns bb-depsolve.harmony.refs-trifecta-test
  "Golden + property + mutation coverage for constant-pool reading.

   The mutation facet pins the two ways this can quietly stop working: a
   pattern that drops the gensym requirement (every stable Clojure class
   becomes a false finding), and one that only sees `clojure/core` (every
   nested namespace stops being seen at all)."
  (:require [bb-depsolve.harmony.refs :as refs]
            [bb-depsolve.harmony.schema :as schema]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

;; =============================================================================
;; Fixtures — constant-pool text as a class file actually carries it
;; =============================================================================

(def ^:private versioned "clojure/core$seq_QMARK___5492")
(def ^:private stable "clojure/core$chunked_seq_QMARK_")
(def ^:private nested "clojure/core/async$pipe__1234")
(def ^:private foreign "clojure/data/json$fn__1009")

(defn- pool
  "Class-file-ish text: names appear bare and inside field descriptors, which
   is how a constant pool actually spells them."
  [& names]
  (str "Êþº¾\u0000\u0000"
       (str/join "\u0001" (mapcat (fn [n] [n (str "L" n ";")]) names))))

(def ^:private gen-pool
  "Text built from a mix of versioned, stable and foreign names, plus binary
   noise — the generator must be able to produce the EMPTY answer too, or the
   property never exercises the no-match branch."
  (gen/let [names (gen/vector (gen/elements [versioned stable nested foreign]) 0 6)
            noise gen/string]
    (str noise (apply pool names))))

(defn- all-versioned?
  "Every ref the reader returns must itself be a versioned class name — the
   invariant the `:harmony/class-ref` schema states."
  [refs]
  (every? #(re-matches schema/versioned-class-re %) refs))

;; =============================================================================
;; Trifecta
;; =============================================================================

(tri/deftrifecta class-refs-trifecta
  bb-depsolve.harmony.refs/class-refs
  {:golden-path "test/golden/bb-depsolve/harmony-class-refs.edn"
   :cases {:versioned-only (pool versioned)
           :stable-is-not-versioned (pool stable)
           :nested-namespace (pool nested)
           :foreign-library (pool foreign)
           :mixed (pool versioned stable nested foreign)
           :empty ""}
   :gen gen-pool
   :pred all-versioned?
   :num-tests 300
   :mutations [["drops-the-gensym-requirement"
                (fn [text]
                  (into #{} (re-seq #"clojure/[a-zA-Z0-9_/]+\$[a-zA-Z0-9_$]*" (str text))))]
               ["only-sees-clojure-core"
                (fn [text]
                  (into #{} (re-seq #"clojure/core\$[a-zA-Z0-9_$]*__\d+" (str text))))]]})

;; =============================================================================
;; What a schema cannot state
;; =============================================================================

(deftest namespace-ownership
  (testing "a ref's namespace is everything before the first $"
    (is (= "clojure/core" (refs/namespace-path versioned)))
    (is (= "clojure/core/async" (refs/namespace-path nested)))
    (is (= "clojure/data/json" (refs/namespace-path foreign))))

  (testing "an artifact owns the namespaces it emitted __init classes for"
    (is (= #{"clojure/core" "clojure/set"}
           (refs/init-namespaces ["clojure/core__init.class"
                                  "clojure/set__init.class"
                                  "clojure/core$assoc__1.class"
                                  "clojure/core.clj"]))))

  (testing "ownership narrows demands to the artifact under test"
    (is (= #{versioned}
           (refs/owned-by #{versioned foreign} #{"clojure/core"}))
        "a reference to another library says nothing about Clojure's version")))

(deftest aot-detection
  (testing "__init.class is the tell, and nothing else is"
    (is (refs/aot-output? ["a/b__init.class"]))
    (is (not (refs/aot-output? ["a/b.clj" "META-INF/MANIFEST.MF"])))
    (is (not (refs/aot-output? ["com/example/Plain.class"]))
        "a plain Java jar is not AOT output and must never be scanned")))

(deftest missing-subtracts-what-the-jar-carries
  (testing "a jar carrying its own copy of a class is not missing it"
    (is (empty? (refs/missing #{versioned} #{} #{versioned})))
    (is (= [versioned] (vec (refs/missing #{versioned} #{} #{}))))
    (is (empty? (refs/missing #{versioned} #{versioned} #{})))))
