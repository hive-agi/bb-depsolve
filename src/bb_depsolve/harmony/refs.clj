(ns bb-depsolve.harmony.refs
  "Promote: class bytes -> the versioned Clojure classes a jar demands.

   Pure. Everything here takes text and sets and returns sets, so the whole
   layer is testable without a jar, a classpath or a filesystem. The adapter
   that turns bytes into the text these functions read is
   `bb-depsolve.harmony.store`."
  (:require [bb-depsolve.harmony.schema :as schema]
            [clojure.string :as str]
            [malli.core :as m]))

(def ^:private class-suffix ".class")

(defn class-refs
  "Every versioned Clojure class name TEXT mentions, as a set.

   TEXT is the ISO-8859-1 view of a class file — the encoding that maps every
   byte to exactly one char, so nothing is lost or merged on the way in. A
   class name is plain ASCII in both the class-ref and the descriptor
   spellings (`Lclojure/core$f__1;`), so one regex over that view sees both.

   Reading the constant pool's UTF-8 region directly rather than through a
   bytecode library is deliberate: babashka has no ASM, and this is the only
   fact the check needs from a class file."
  [text]
  (into #{} (re-seq schema/versioned-class-re (str text))))

(defn entry-class-names
  "The class entry names in ENTRY-NAMES, stripped of `.class`. What a jar
   PROVIDES."
  [entry-names]
  (into #{} (comp (filter #(str/ends-with? % class-suffix))
                  (map #(subs % 0 (- (count %) (count class-suffix)))))
        entry-names))

(defn class-entry?
  "Does ENTRY-NAME name a class file?"
  [entry-name]
  (str/ends-with? (str entry-name) class-suffix))

(defn aot-output?
  "Do ENTRY-NAMES carry Clojure AOT output?

   The tell is `<ns>__init.class`: the Clojure compiler emits exactly one per
   namespace it AOT-compiles, and nothing else produces that suffix. A pure
   Java jar has none, which is what lets a whole classpath be triaged from the
   zip central directory alone — the per-class read only happens for jars that
   answer yes here."
  [entry-names]
  (boolean (some #(str/ends-with? (str %) (str "__init" class-suffix)) entry-names)))

(defn namespace-path
  "The namespace part of CLASS-REF: everything before the first `$`.
   `clojure/core$assoc__5502` => `clojure/core`."
  [class-ref]
  (let [s (str class-ref)
        i (str/index-of s "$")]
    (if i (subs s 0 i) s)))

(defn init-namespaces
  "The namespaces ENTRY-NAMES carries AOT output for, as paths.
   `clojure/core__init.class` => `clojure/core`.

   This is how an artifact says which namespaces are ITS OWN."
  [entry-names]
  (let [suffix (str "__init" class-suffix)]
    (into #{}
          (comp (filter #(str/ends-with? (str %) suffix))
                (map #(subs (str %) 0 (- (count (str %)) (count suffix)))))
          entry-names)))

(defn clojure-artifact?
  "Is this the Clojure artifact itself, judged from ENTRY-NAMES?

   It owns `clojure/core`. This matters because a Clojure jar IS AOT output,
   so a rehearsal that puts a new Clojure beside the old one would otherwise
   scan the old one as a consumer — a jar demanding its own classes, which it
   trivially supplies. It is the SUBJECT of the check, never a party to it."
  [entry-names]
  (contains? (init-namespaces entry-names) "clojure/core"))

(defn owned-by
  "The refs in REQUIRED whose namespace is one of NAMESPACES.

   This is what keeps the question honest. A jar's constant pool names classes
   from every library it was compiled against, and only the ones belonging to
   the artifact UNDER TEST say anything about that artifact's version. Without
   this narrowing, ClojureScript's baked-in references to `clojure.data.json`
   read as Clojure classes gone missing — a finding about a library nobody was
   asking about, drowning the one that matters."
  [required namespaces]
  (into #{} (filter #(contains? namespaces (namespace-path %))) required))

(defn missing
  "The classes REQUIRED that PROVIDED does not have, sorted.

   SELF-PROVIDED is subtracted as well, so a jar carrying its own copy of a
   class is never reported as missing it."
  [required provided self-provided]
  (into (sorted-set)
        (remove #(or (contains? provided %) (contains? self-provided %)))
        required))

(m/=> class-refs [:=> [:cat :string] :harmony/class-refs])
(m/=> entry-class-names [:=> [:cat [:sequential :string]] [:set :string]])
(m/=> class-entry? [:=> [:cat :string] :boolean])
(m/=> aot-output? [:=> [:cat [:sequential :string]] :boolean])
(m/=> missing [:=> [:cat [:set :string] [:set :string] [:set :string]] [:sequential :string]])
(m/=> namespace-path [:=> [:cat :string] :string])
(m/=> init-namespaces [:=> [:cat [:sequential :string]] [:set :string]])
(m/=> clojure-artifact? [:=> [:cat [:sequential :string]] :boolean])
(m/=> owned-by [:=> [:cat [:set :string] [:set :string]] [:set :string]])
