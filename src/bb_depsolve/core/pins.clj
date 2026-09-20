(ns bb-depsolve.core.pins
  "Pins: a dependency held at a version by the workspace, not by a flag.

   File format, at the workspace root:

     {:pins [{:lib     org.clojure/core.cache
              :version \"1.2.263\"
              :project \"hive-cache\"
              :reason  \"single-flight test flakes above this\"
              :since   \"2026-09-20\"}]}

   Only :lib is required. A bare `foo` names `foo/foo`, matching the --only
   and --exclude CSV grammar.

   :project scopes a pin to one project; a project-scoped pin wins over a
   workspace-wide one for the same lib. :version records where the pin is
   held, which is what lets a pin be reported as drifted once the dep file
   moves underneath it.

   pin       {:lib :version :project :reason :since}
   drift-row {:pin :lib :project :pinned-version :actual-version :status}
               :status  :held | :drifted | :unused"
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def default-file-name "depsolve-pins.edn")

;; =============================================================================
;; Parsing (pure)
;; =============================================================================

(defn normalize-lib
  "LIB as a qualified symbol. A bare `foo` names `foo/foo`; nil stays nil."
  [lib]
  (when lib
    (let [s (str lib)]
      (symbol (if (str/includes? s "/") s (str s "/" s))))))

(defn normalize-pin
  "One raw pin entry with :lib qualified and :project stringified.
   Returns nil when the entry names no lib."
  [{:keys [lib project] :as pin}]
  (when-let [lib (normalize-lib lib)]
    (cond-> (assoc pin :lib lib)
      project (assoc :project (str project)))))

(defn parse
  "Raw pins edn -> {:entries [pin] :index {[lib project-or-nil] pin}}.

   Entries naming no lib are dropped. A later entry with the same lib and
   project replaces an earlier one, so the file reads top to bottom."
  [{:keys [pins]}]
  (let [entries (vec (keep normalize-pin pins))]
    {:entries entries
     :index   (into {} (map (juxt (juxt :lib :project) identity)) entries)}))

(defn pin-for
  "The pin covering LIB in PROJECT, or nil. A pin scoped to PROJECT wins over
   a workspace-wide pin for the same lib."
  [pins lib project]
  (when pins
    (let [lib (normalize-lib lib)]
      (or (get-in pins [:index [lib (some-> project str)]])
          (get-in pins [:index [lib nil]])))))

(defn pinned?
  "True when a pin covers LIB in PROJECT."
  [pins lib project]
  (some? (pin-for pins lib project)))

;; =============================================================================
;; Auditing a pin against what the workspace actually declares (pure)
;; =============================================================================

(defn- covered-deps
  "The DEPS ({:lib :project :version}) one PIN covers."
  [pin deps]
  (filterv (fn [{:keys [lib project]}]
             (and (= (:lib pin) (normalize-lib lib))
                  (or (nil? (:project pin)) (= (:project pin) (str project)))))
           deps))

(defn audit
  "Every pin in PINS against DEPS ({:lib :project :version}), as [drift-row].

   A pin is
     :unused   when no dep file declares the lib it names,
     :drifted  when it records a :version and a covered dep is pinned at some
               other version, and
     :held     otherwise.

   A pin with no :version can never drift: it forbids movement without
   asserting where the dep sits. One row per covered dep, so a workspace-wide
   pin reports per project; an :unused pin yields a single row."
  [pins deps]
  (vec
   (for [pin (:entries pins)
         :let [covered (covered-deps pin deps)]
         row (if (seq covered) covered [nil])]
     (let [actual (:version row)]
       {:pin     pin
        :lib     (:lib pin)
        :project (or (:project row) (:project pin))
        :pinned-version (:version pin)
        :actual-version actual
        :status  (cond
                   (nil? row) :unused
                   (and (:version pin) (not= (:version pin) actual)) :drifted
                   :else :held)}))))

(defn status-counts
  "{status -> count} over an `audit` result, for a one-line summary."
  [rows]
  (reduce (fn [acc {:keys [status]}] (update acc status (fnil inc 0))) {} rows))

;; =============================================================================
;; Reading the file (boundary)
;; =============================================================================

(defn pins-path
  "Where the pins file lives under ROOT."
  [root]
  (str (fs/path root default-file-name)))

(defn read-pins
  "The parsed pins under ROOT, or nil when no pins file exists."
  [root]
  (let [p (pins-path root)]
    (when (fs/exists? p)
      (parse (edn/read-string (slurp p))))))
