(ns bb-depsolve.harmony.api-test
  "The boundary, driven entirely through stub ports.

   Every fact this subsystem reports comes from three ports, so a test that
   implements them needs no jar, no ~/.m2 and no network — and the stubs are
   held to the SAME contract the real adapters are: `entry-names` and
   `project-jars` answer nil for \"could not read\", never an empty collection."
  (:require [bb-depsolve.harmony.api :as api]
            [clojure.string :as str]
            [bb-depsolve.harmony.plan :as plan]
            [bb-depsolve.harmony.port :as port]
            [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Stub ports
;; =============================================================================

(defrecord MapJarReader [jars]
  ;; jars: {jar-path {entry-name text-or-nil}}
  port/IJarReader
  (entry-names [_ jar] (some-> (get jars jar) keys vec))
  (class-texts [_ jar]
    (when-let [entries (get jars jar)]
      (vec (for [[n t] entries :when (and t (str/ends-with? n ".class"))] [n t])))))

(defrecord MapResolver [by-coord]
  port/IArtifactResolver
  (local-jar [_ lib version] (get by-coord [lib version]))
  (resolve-jar [_ lib version] (get by-coord [lib version])))

(defrecord MapClasspath [by-dir]
  port/IClasspathSource
  (project-jars [_ dir] (get by-dir dir)))

;; =============================================================================
;; Fixtures — two Clojures that renumber one class, and a jar built on one
;; =============================================================================

(def ^:private old-ref "clojure/core$seq_QMARK___5492")
(def ^:private new-ref "clojure/core$seq_QMARK___5490")

(defn- clojure-jar
  "A Clojure artifact providing REF: an __init class (which is how it says it
   OWNS clojure/core) plus the fn class itself."
  [ref]
  {"clojure/core__init.class" ""
   (str ref ".class") ""})

(def ^:private aot-consumer
  "A third-party jar that AOT-compiled its own namespace against the OLD
   Clojure — the deep-diamond shape."
  {"lib/thing__init.class" (str "L" old-ref ";")
   "lib/thing$go__9.class" (str "some noise " old-ref " more noise")})

(def ^:private plain-jar
  {"com/example/Plain.class" "nothing clojure here"
   "META-INF/MANIFEST.MF" ""})

(def ^:private store
  {:reader (->MapJarReader {"clj-old.jar" (clojure-jar old-ref)
                            "clj-new.jar" (clojure-jar new-ref)
                            "consumer.jar" aot-consumer
                            "plain.jar" plain-jar})
   :resolver (->MapResolver {['org.clojure/clojure "1.0"] "clj-old.jar"
                             ['org.clojure/clojure "2.0"] "clj-new.jar"
                             ['acme/consumer "1.0"] "consumer.jar"
                             ['acme/plain "1.0"] "plain.jar"})
   :classpath (->MapClasspath {"/proj" ["clj-old.jar" "consumer.jar" "plain.jar"]})})

;; =============================================================================
;; Scanning
;; =============================================================================

(deftest scanning-triages-before-it-reads
  (testing "a plain Java jar is not AOT output and demands nothing"
    (let [scan (api/scan-jar (:reader store) "plain.jar")]
      (is (false? (:aot? scan)))
      (is (nil? (:required scan)) "a non-AOT jar's classes are never read")))

  (testing "an AOT jar's demands come out of its class text"
    (let [scan (api/scan-jar (:reader store) "consumer.jar")]
      (is (true? (:aot? scan)))
      (is (= #{old-ref} (:required scan)))))

  (testing "an unreadable jar is a gap, not a disagreement"
    (let [scan (api/scan-jar (:reader store) "absent.jar")]
      (is (false? (:aot? scan)))
      (is (some? (:unreadable scan))))))

(deftest subject-is-classes-plus-owned-namespaces
  (let [subj (api/subject (:reader store) "clj-old.jar")]
    (is (= #{"clojure/core"} (:namespaces subj)))
    (is (contains? (:classes subj) old-ref))))

;; =============================================================================
;; The two questions
;; =============================================================================

(deftest check-project-sees-the-renumbering
  (testing "the Clojure it was built against agrees"
    (is (= :agreed (:verdict (api/check-project store "/proj" "1.0")))))

  (testing "the one that renumbers the class breaks it, and names the jar"
    (let [r (api/check-project store "/proj" "2.0")]
      (is (= :broken (:verdict r)))
      (is (= ["consumer.jar"] (mapv :jar (:findings r))))
      (is (= [old-ref] (:missing (first (:findings r)))))
      (is (= 1 (:aot-jars r))
          "the plain jar is not AOT, and the OLD Clojure still on the classpath
           is the subject being replaced, not a party to the check")))

  (testing "a Clojure artifact is recognised and never scanned as a consumer"
    (let [scan (api/scan-jar (:reader store) "clj-old.jar")]
      (is (true? (:aot? scan)) "a Clojure jar is itself AOT output")
      (is (true? (:clojure-artifact? scan)))
      (is (nil? (:required scan))
          "its classes are never read: it would trivially demand what it supplies")))

  (testing "an unresolvable classpath is unverified, never agreement"
    (let [r (api/check-project store "/nowhere" "1.0")]
      (is (= :unverified (:verdict r)))
      (is (= :classpath-unresolvable (:reason r)))
      (is (false? (plan/broken? r)))))

  (testing "a Clojure jar that cannot be fetched is unverified too"
    (is (= :clojure-jar-unavailable
           (:reason (api/check-project store "/proj" "99.0"))))))

(deftest check-artifact-is-the-mirror
  (testing "moving the jar under a fixed Clojure asks the same question"
    (is (= :agreed (:verdict (api/check-artifact store 'acme/consumer "1.0" "1.0"))))
    (is (= :broken (:verdict (api/check-artifact store 'acme/consumer "1.0" "2.0")))))

  (testing "a non-AOT artifact cannot break harmony and is not judged"
    (is (nil? (api/check-artifact store 'acme/plain "1.0" "2.0")))))

;; =============================================================================
;; What the upgrade path depends on
;; =============================================================================

(deftest check-row-holds-only-on-evidence
  (let [row {:lib 'org.clojure/clojure :old-version "1.0" :new-version "2.0"
             :project "p" :path "/proj/deps.edn"}]
    (testing "a real break holds the row and carries it for the report"
      (let [r (api/check-row store row "/proj" "1.0")]
        (is (= :broken (:verdict r)))
        (is (= row (:row r)))))

    (testing "a blind check does NOT hold the row"
      (is (nil? (api/check-row store row "/nowhere" "1.0"))
          "an offline run must not hold the whole workspace"))

    (testing "a harmless move is not held"
      (is (nil? (api/check-row store (assoc row :new-version "1.0") "/proj" "1.0"))))))

(deftest relevant-row-triage-is-free
  (testing "the Clojure coordinate is always worth checking"
    (is (api/relevant-row? store {:lib 'org.clojure/clojure :old-version "1.0"})))

  (testing "a lib whose current jar is not AOT is dismissed without a download"
    (is (not (api/relevant-row? store {:lib 'acme/plain :old-version "1.0"}))
        "triage reads ~/.m2 only; a triage that downloads has no reason to exist")))
