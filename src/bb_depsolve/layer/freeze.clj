(ns bb-depsolve.layer.freeze
  "Release-cadence budget for frozen layers.

   A layer marked :frozen? in the layer table declares a maximum number of
   releases per window. A contract that ships faster than its budget forces
   every consumer to re-pin on a change that was supposed to be stable, so
   exceeding the budget is reported alongside layer violations."
  (:require [bb-depsolve.core.git :as git]
            [clojure.string :as str]))

(def default-window-days 90)

(defn tag-timestamps
  "Unix creation timestamps of every tag in PROJECT-DIR, newest first.
   Empty when the directory is not a git repo."
  [project-dir]
  (let [res (git/git project-dir "for-each-ref" "--sort=-creatordate"
                     "--format=%(creatordate:unix)" "refs/tags")]
    (if (zero? (:exit res))
      (->> (str/split-lines (:out res))
           (remove str/blank?)
           (keep #(parse-long (str/trim %)))
           vec)
      [])))

(defn releases-within
  "How many of TIMESTAMPS fall within WINDOW-DAYS before NOW-UNIX."
  [timestamps now-unix window-days]
  (let [cutoff (- now-unix (* window-days 86400))]
    (count (filter #(> (long %) cutoff) timestamps))))

(defn over-budget?
  "True when RELEASES exceeds BUDGET. A nil budget is unlimited."
  [releases budget]
  (boolean (and budget (> (long releases) (long budget)))))

(defn frozen-projects
  "Projects TABLE ranks into a frozen layer, as
   [{:project :level :budget} ...]."
  [table]
  (vec (for [[project level] (sort (:levels table))
             :let [budget (get (:frozen table) level)]
             :when budget]
         {:project project :level level :budget budget})))

(defn check
  "Cadence report for every frozen project.

   NODES is {project {:dir ...}} — the :nodes slot of an internal graph.
   Returns [{:project :budget :releases :window-days :over?} ...], ordered
   worst first."
  [table nodes now-unix window-days]
  (->> (frozen-projects table)
       (keep (fn [{:keys [project budget]}]
               (when-let [dir (get-in nodes [project :dir])]
                 (let [n (releases-within (tag-timestamps dir) now-unix window-days)]
                   {:project     project
                    :budget      budget
                    :releases    n
                    :window-days window-days
                    :over?       (over-budget? n budget)}))))
       (sort-by (juxt (complement :over?) (comp - :releases)))
       vec))

(defn breaches
  "Only the entries of a `check` report that are over budget."
  [report]
  (filterv :over? report))
