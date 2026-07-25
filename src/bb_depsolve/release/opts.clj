(ns bb-depsolve.release.opts
  (:require [bb-depsolve.core.bump :as bump]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.cli.ui :as ui]
            [clojure.string :as str]))

(declare skip-set requested-bump parse-seeds require-org!)

(defn skip-set
  [skip-dirs]
  (if skip-dirs
    (into #{} (str/split skip-dirs #","))
    discovery/default-skip-dirs))

(defn requested-bump
  "Bump kind a set of CLI flags asks for. One mapping for the whole tool:
   bb-depsolve.core.bump/bump-level."
  [opts]
  (bump/bump-level opts))

(defn parse-seeds
  "Seed project names from a comma-separated --from value. Nil when absent."
  [from]
  (when (and from (not (str/blank? from)))
    (into (sorted-set) (remove str/blank?) (map str/trim (str/split from #",")))))

(defn require-org!
  [org cmd]
  (when-not org
    (println (ui/c :red (format "Error: --org is required for %s (e.g. --org hive-agi)" cmd)))
    (System/exit 1)))
