(ns bb-depsolve.core.pins.cmd
  "The pins command: list what the workspace holds back, and why."
  (:require [babashka.fs :as fs]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.core.pins :as pins]
            [clojure.string :as str]))

(defn workspace-deps
  "Every mvn dep the workspace declares, as [{:lib :project :version :path}].
   The same discovery `upgrade` walks, so a pin is audited against exactly
   what upgrade would have moved."
  [{:keys [root skip-dirs depth project]}]
  (let [dep-files (cond->> (discovery/find-dep-files {:root root
                                                      :skip-dirs skip-dirs
                                                      :depth depth})
                    project (filter #(= project (:project %))))]
    (vec (for [{:keys [path project] :as dep-file} dep-files
               :let [content (slurp path)]
               {:keys [lib version]} (discovery/extract-mvn-deps dep-file content)]
           {:lib lib :project project :version version :path path}))))

(defn- status-colour
  [status]
  (case status
    :held    :green
    :drifted :yellow
    :unused  :dim
    :dim))

(defn- status-note
  [{:keys [status pinned-version actual-version]}]
  (case status
    :held    (if pinned-version "held" "held (no version recorded)")
    :drifted (str "DRIFTED: pinned " pinned-version ", file says " actual-version)
    :unused  "unused: no dep file declares this lib"
    (str status)))

(defn print-rows!
  "Print an `audit` result, one line per row, worst last so the tail of the
   output is what needs attention. Returns the count of rows needing it."
  [rows]
  (let [ordered (sort-by (juxt #(case (:status %) :held 0 :unused 1 :drifted 2)
                               (comp str :lib)
                               (comp str :project))
                         rows)]
    (doseq [{:keys [lib project pin status] :as row} ordered]
      (printf "  %-40s %-18s %s\n"
              (str lib)
              (ui/c :dim (or project "*"))
              (ui/c (status-colour status) (status-note row)))
      (when-let [reason (:reason pin)]
        (println (ui/c :dim (str "      " reason))))
      (when-let [since (:since pin)]
        (println (ui/c :dim (str "      pinned since " since)))))
    (count (remove #(= :held (:status %)) ordered))))

(defn print-missing!
  "Say where a pins file would go, and what one looks like."
  [root-dir]
  (println (ui/c :dim (str "  No " pins/default-file-name " at " root-dir ".")))
  (println)
  (println (ui/c :dim "  A pins file holds a dep where it is, with the reason attached:"))
  (println)
  (println (ui/c :dim "    {:pins [{:lib     org.clojure/core.cache"))
  (println (ui/c :dim "             :version \"1.2.263\""))
  (println (ui/c :dim "             :project \"hive-cache\""))
  (println (ui/c :dim "             :reason  \"why this is held\""))
  (println (ui/c :dim "             :since   \"2026-09-20\"}]}"))
  (println)
  (println (ui/c :dim "  `upgrade` then reports the lib as HELD with that reason,"))
  (println (ui/c :dim "  instead of offering it again on every run.")))

(defn pins-cmd
  "List the pins the workspace records, and audit each against what the dep
   files actually declare.

   A pin is reported :held when the dep sits where the pin says, :drifted when
   the file moved underneath it, and :unused when no dep file declares the lib
   at all. Both of the latter mean the pins file has gone stale, so --no-fail
   is what a reporting run passes.

   Flags:
     --root <dir>     workspace root (default .)
     --project <name> audit pins against one project only
     --no-fail        report drift without exiting 1"
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs depth project no-fail]
         :or {root "." depth discovery/default-depth}} opts
        root-dir (str (fs/canonicalize root))
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)]
    (println (ui/c :bold "Pins recorded for this workspace"))
    (println)
    (if-let [p (pins/read-pins root-dir)]
      (let [rows (pins/audit p (workspace-deps {:root root :skip-dirs skip-set
                                                :depth depth :project project}))
            counts (pins/status-counts rows)
            stale (print-rows! rows)]
        (println)
        (println (ui/c :dim (format "  %d pin(s): %d held, %d drifted, %d unused."
                                    (count (:entries p))
                                    (get counts :held 0)
                                    (get counts :drifted 0)
                                    (get counts :unused 0))))
        (when (and (pos? stale) (not no-fail))
          (System/exit 1)))
      (print-missing! root-dir))))
