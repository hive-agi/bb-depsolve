(ns bb-depsolve.cli.ui
  (:require [clojure.string :as str]
            [babashka.process :as proc]
            [bblgum.core :as gum]))

(declare c colors tty? visible-len pad-right)

(defn c
  "Public: wrap STRING in ANSI color code. Used by sibling nses (audit, etc)."
  [color & parts]
  (str (get colors color "") (apply str parts) (:reset colors)))

(defn gum-table [csv multi-project all-projects]
  (if (tty?)
    (gum/gum :table :in (.getBytes csv))
    (let [lib-col 45
          ver-col 13
          abbrev (fn [s w] (subs s 0 (min w (count s))))]
      (print (pad-right "Library" lib-col))
      (doseq [p all-projects]
        (print "  " (pad-right (abbrev p ver-col) ver-col)))
      (println)
      (println (apply str (repeat (+ lib-col (* (+ 2 ver-col) (count all-projects))) \-)))
      (doseq [[lib project-versions] multi-project
              :let [versions (set (vals project-versions))
                    drift? (> (count versions) 1)]]
        (print (pad-right (str lib) lib-col))
        (doseq [p all-projects
                :let [v* (get project-versions p)
                      display (if v* (abbrev v* ver-col) "-")
                      colored (cond
                                (nil? v*) (c :dim "-")
                                drift?    (c :yellow display)
                                :else     (c :dim display))]]
          (print "  " (pad-right colored ver-col)))
        (println)))))

(defn gum-filter [choices header]
  (when (tty?)
    (let [{:keys [status result]} (gum/gum :filter choices
                                           :no-limit true
                                           :header header)]
      (when (= 0 status) result))))

(defn pad-right [s width]
  (let [vlen (visible-len s)
        padding (max 0 (- width vlen))]
    (str s (apply str (repeat padding \space)))))

(defn visible-len [s]
  (count (str/replace s #"\033\[[0-9;]*m" "")))

(defn matrix->csv [multi-project all-projects]
  (let [header (str/join "," (cons "Library" all-projects))
        rows (for [[lib project-versions] multi-project]
               (str/join ","
                         (cons (str lib)
                               (for [p all-projects]
                                 (or (get project-versions p) "-")))))]
    (str/join "\n" (cons header rows))))

(defn tty? []
  (zero? (:exit (proc/sh ["test" "-t" "0"] {:continue true}))))

(def ^:private colors
  {:red     "\033[31m"
   :green   "\033[32m"
   :yellow  "\033[33m"
   :cyan    "\033[36m"
   :bold    "\033[1m"
   :dim     "\033[2m"
   :reset   "\033[0m"})

(defn format-local-dep-warning
  "Format a warning line for a :local/root dep."
  [project lib path]
  (format "  %-25s %-35s %s"
          (c :cyan project) (str lib) (c :yellow path)))