(ns bb-depsolve.harmony.api
  "Boundary: does a proposed version still agree with the AOT jars around it?

   A jar that ships AOT-compiled Clojure classes has the exact class NAMES of
   the Clojure it was built against baked into its constant pool, gensym
   suffixes and all:

     clojure/core$seq_QMARK___5492   under Clojure 1.12.5
     clojure/core$seq_QMARK___5490   under Clojure 1.12.6

   so such a jar dies with NoClassDefFoundError the moment the Clojure
   coordinate moves, before a single namespace of the consumer finishes
   loading. No version number expresses that constraint. Only the bytes do,
   and this is where they are read.

   Effects enter through the three ports in `bb-depsolve.harmony.port`; the
   decisions are made in `plan` and `rules`, which never touch a file."
  (:require [bb-depsolve.harmony.plan :as plan]
            [bb-depsolve.harmony.port :as port]
            [bb-depsolve.harmony.refs :as refs]
            [bb-depsolve.harmony.store :as store]
            [malli.core :as m]))

(def clojure-lib 'org.clojure/clojure)

;; =============================================================================
;; Collect
;; =============================================================================

(defn scan-jar
  "Read JAR through READER: what it demands and what it supplies.

   A jar that is not AOT output short-circuits after the entry NAMES, which the
   zip central directory already holds — nothing is decompressed. An unreadable
   jar reports :aot? false and carries :unreadable: a classpath entry we cannot
   open is a gap in the check, which `plan/report` counts separately, never a
   disagreement."
  [reader jar]
  (if-let [names (port/entry-names reader jar)]
    (cond
      (not (refs/aot-output? names)) {:jar jar :aot? false}
      (refs/clojure-artifact? names) {:jar jar :aot? true :clojure-artifact? true}
      :else
      (if-let [texts (port/class-texts reader jar)]
        {:jar jar
         :aot? true
         :self-provided (refs/entry-class-names names)
         :required (into #{} (mapcat (fn [[_ text]] (refs/class-refs text))) texts)}
        {:jar jar :aot? false :unreadable "class entries unreadable"}))
    {:jar jar :aot? false :unreadable "jar unreadable"}))

(defn subject
  "JAR as the artifact a check is ABOUT: the classes it ships and the
   namespaces it owns, through READER. nil when it cannot be read.

   Ownership comes from the `__init.class` entries — an artifact's AOT output
   is the only self-description of which namespaces are its own, and it is what
   keeps a finding about Clojure from being a finding about data.json."
  [reader jar]
  (when-let [names (port/entry-names reader jar)]
    {:classes (refs/entry-class-names names)
     :namespaces (refs/init-namespaces names)}))

;; =============================================================================
;; Boundary — the two questions callers ask
;; =============================================================================

(defn check-project
  "Would PROJECT-DIR survive pinning org.clojure/clojure at CLOJURE-VERSION?

   Reads the project's committed classpath, scans every AOT jar on it, and
   diffs their versioned class references against that Clojure jar's own
   entries. Returns a `:harmony/report`; a classpath or Clojure jar that could
   not be read yields verdict :unverified with a :reason, which is NOT
   agreement."
  ([project-dir clojure-version] (check-project (store/default-store) project-dir clojure-version))
  ([{:keys [reader resolver classpath]} project-dir clojure-version]
   (let [jars (port/project-jars classpath project-dir)
         clj-jar (port/resolve-jar resolver clojure-lib clojure-version)
         subj (some->> clj-jar (subject reader))]
     (-> (cond
           (nil? jars) (plan/blind :classpath-unresolvable)
           (nil? subj) (plan/blind :clojure-jar-unavailable)
           :else (plan/report (mapv #(scan-jar reader %) jars) subj))
         (assoc :project-dir (str project-dir)
                :clojure-version clojure-version)))))

(defn check-artifact
  "Would LIB at VERSION agree with Clojure CLOJURE-VERSION?

   The mirror of `check-project`: instead of moving Clojure under a fixed set
   of jars, it moves one jar under a fixed Clojure. nil when LIB at VERSION is
   not AOT output and so cannot break harmony at all."
  [{:keys [reader resolver]} lib version clojure-version]
  (when-let [jar (port/resolve-jar resolver lib version)]
    (let [scan (scan-jar reader jar)]
      (when (:aot? scan)
        (if-let [subj (some->> (port/resolve-jar resolver clojure-lib clojure-version)
                               (subject reader))]
          (assoc (plan/report [scan] subj) :clojure-version clojure-version)
          (assoc (plan/blind :clojure-jar-unavailable) :clojure-version clojure-version))))))

(defn aot-artifact?
  "Is LIB at VERSION AOT output, judged from what is already in ~/.m2?

   Deliberately does NOT fetch: this is the triage that decides whether a row
   is worth a download at all, and a triage that downloads has no reason to
   exist."
  [{:keys [reader resolver]} lib version]
  (boolean (some-> (port/local-jar resolver lib version)
                   (->> (port/entry-names reader))
                   refs/aot-output?)))

(defn relevant-row?
  "Is ROW worth a classpath read at all?

   Yes for the Clojure coordinate itself, and for a lib whose CURRENT jar is
   already AOT output — a library shipping compiled classes today ships them at
   the next version too, and it is the only other kind of row that can break
   harmony. Every other row is decided from ~/.m2 alone, with no download and
   no classpath resolve, which is what keeps this check off the critical path
   of an upgrade run over hundreds of libs."
  ([row] (relevant-row? (store/default-store) row))
  ([store {:keys [lib old-version]}]
   (or (= lib clojure-lib)
       (boolean (and old-version (aot-artifact? store lib old-version))))))

(defn check-row
  "The harmony report holding ROW back, or nil when ROW is harmless.

   Two directions, one check:

     the Clojure coordinate moves  — every AOT jar already on the classpath is
                                     re-checked against the new Clojure jar
     an AOT jar moves              — the NEW jar is checked against the Clojure
                                     the project pins today

   ROW is an upgrade row ({:project :path :lib :old-version :new-version});
   CLOJURE-VERSION is what the project pins today. Returns nil when nothing was
   found AND when nothing could be read — an unverified check must not hold a
   row, or an offline run would hold the whole workspace."
  ([row project-dir clojure-version]
   (check-row (store/default-store) row project-dir clojure-version))
  ([store {:keys [lib new-version] :as row} project-dir clojure-version]
   (let [report (if (= lib clojure-lib)
                  (check-project store project-dir new-version)
                  (when clojure-version
                    (check-artifact store lib new-version clojure-version)))]
     (when (and report (plan/broken? report))
       (assoc report :row row)))))

(m/=> scan-jar [:=> [:cat :any :string] :harmony/jar-scan])
(m/=> aot-artifact? [:=> [:cat :map :symbol :string] :boolean])
