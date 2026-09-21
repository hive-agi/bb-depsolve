(ns bb-depsolve.harmony.schema
  "Value objects of AOT harmony. Self-registering.

   The ubiquitous language of this subsystem:

     class-ref   a versioned Clojure class NAME, as it appears in a constant
                 pool: `clojure/core$seq_QMARK___5492`. Versioned means it
                 carries the compiler's gensym suffix, so it names one
                 particular Clojure release and no other.
     jar-scan    what one jar demands and supplies, read from its bytes.
     finding     one jar whose demands a given Clojure cannot meet.
     report      the verdict over a set of scans."
  (:require [hive-schemas.schema :as hs]))

(def versioned-class-re
  "A Clojure fn class carrying a compile-time gensym suffix.

   `clojure/core$seq_QMARK___5492` is `seq_QMARK_` plus `__5492`. The suffix is
   assigned in source order, so it moves on any release that adds or removes a
   form ahead of it — which is exactly what makes such a reference version-
   specific. Names WITHOUT a numeric suffix (`clojure/core$chunked_seq_QMARK_`)
   are stable across releases and deliberately fall outside this schema."
  #"clojure/[a-zA-Z0-9_/]+\$[a-zA-Z0-9_$]*__\d+")

(def schemas
  {:harmony/class-ref [:re versioned-class-re]

   :harmony/class-refs [:set :harmony/class-ref]

   :harmony/jar-path :string

   ;; :required is what the jar's bytes NAME; :self-provided what it ships
   ;; itself. A jar that AOT-compiled clojure.core carries its own copies, and
   ;; those resolve from the jar whatever the Clojure coordinate says — so the
   ;; two sets are kept apart rather than pre-subtracted.
   :harmony/jar-scan
   [:map
    [:jar :harmony/jar-path]
    [:aot? :boolean]
    [:clojure-artifact? {:optional true} :boolean]
    [:required {:optional true} :harmony/class-refs]
    [:self-provided {:optional true} [:set :string]]
    [:unreadable {:optional true} [:maybe :string]]]

   ;; :missing is capped for display; :missing-count is the true total. A
   ;; version skew misses thousands of classes and printing them all buries the
   ;; one line that names the jar.
   :harmony/finding
   [:map
    [:jar :harmony/jar-path]
    [:missing-count [:int {:min 1}]]
    [:missing [:vector :harmony/class-ref]]]

   ;; The artifact a check is ABOUT: the class names it ships and the
   ;; namespaces it owns. Ownership is what narrows a jar's demands to the
   ;; ones this artifact's version can answer for.
   :harmony/subject
   [:map
    [:classes [:set :string]]
    [:namespaces [:set :string]]]

   :harmony/verdict [:enum :agreed :broken :unverified]

   ;; Why a check could not be made. Categorically different from agreement:
   ;; an unread classpath is a blind spot, not a clean bill.
   :harmony/blind-reason [:enum :classpath-unresolvable :clojure-jar-unavailable]

   :harmony/report
   [:map
    [:verdict :harmony/verdict]
    [:findings [:vector :harmony/finding]]
    [:aot-jars [:int {:min 0}]]
    [:unreadable [:vector :harmony/jar-path]]
    [:reason {:optional true} [:maybe :harmony/blind-reason]]
    [:clojure-version {:optional true} [:maybe :string]]
    [:project-dir {:optional true} [:maybe :string]]]

   ;; The context `rules/classify` folds its chain over.
   :harmony/verdict-ctx
   [:map
    [:verified? :boolean]
    [:aot-jars [:int {:min 0}]]
    [:finding-count [:int {:min 0}]]]})

(defonce ^:private registered?
  (delay (hs/register-all! schemas)))

(defn register!
  "Idempotently register the harmony value objects. Returns the keys."
  []
  @registered?)

(register!)
