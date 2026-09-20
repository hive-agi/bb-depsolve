(ns bb-depsolve.core.skip
  "Directories every scan leaves alone, recorded by the workspace.

   File format, at the workspace root:

     {:skip [{:dir    \"clojure-lsp\"
              :reason \"vendored upstream checkout; no push rights\"}]}

   A bare string is accepted in place of the map when no reason is offered.

   This is for a checkout that lives in the workspace but is not the
   workspace's to change: a vendored upstream clone, a scratch copy, a
   read-only mirror. Sweeping one of those writes commits nobody can publish,
   and every push-all after it fails on the same directory.

   --skip-dirs does the same thing for one invocation. The difference is the
   one that matters over time: a flag is re-typed or forgotten, and carries no
   reason, so the next person sees a directory being skipped with no record of
   why."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]))

(def default-file-name "depsolve-skip.edn")

;; =============================================================================
;; Parsing (pure)
;; =============================================================================

(defn normalize-entry
  "One raw skip entry as {:dir string :reason string-or-nil}, or nil when it
   names no directory. A bare string is a dir with no reason."
  [entry]
  (let [m (if (map? entry) entry {:dir entry})
        dir (some-> (:dir m) str not-empty)]
    (when dir
      {:dir dir :reason (some-> (:reason m) str not-empty)})))

(defn parse
  "Raw skip edn -> {:entries [{:dir :reason}] :dirs #{dir}}.
   Entries naming no directory are dropped."
  [{:keys [skip]}]
  (let [entries (vec (keep normalize-entry skip))]
    {:entries entries
     :dirs    (into #{} (map :dir) entries)}))

(defn reason-for
  "Why DIR is skipped, or nil."
  [skip dir]
  (some #(when (= dir (:dir %)) (:reason %)) (:entries skip)))

;; =============================================================================
;; Reading the file (boundary)
;; =============================================================================

(defn skip-path
  "Where the skip file lives under ROOT."
  [root]
  (str (fs/path root default-file-name)))

(defn read-skip
  "The parsed skip list under ROOT, or nil when no skip file exists."
  [root]
  (let [p (skip-path root)]
    (when (fs/exists? p)
      (parse (edn/read-string (slurp p))))))

(defn dirs
  "The set of directory names ROOT's skip file excludes. Empty when there is
   no file, so a caller can union this onto its own skip set unconditionally."
  [root]
  (or (:dirs (read-skip root)) #{}))
