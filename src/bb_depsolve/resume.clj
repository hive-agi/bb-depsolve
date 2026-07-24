(ns bb-depsolve.resume
  "Checkpoint for an interrupted cascade.

   A cascade is not atomic: by the time a later wave fails, earlier waves are
   already tagged and pushed. The run record is written to disk after every
   wave so a re-run can skip what already published instead of re-releasing it.

   record {:status :waves :released {project version}}"
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def record-name "cascade-run.edn")

(defn record-path
  "Where ROOT's cascade checkpoint lives."
  [root]
  (str (fs/path root ".bb-depsolve" record-name)))

(defn save!
  "Write RUN to ROOT's checkpoint. => the path."
  [root run]
  (let [path (record-path root)]
    (fs/create-dirs (fs/parent path))
    (spit path (pr-str run))
    path))

(defn load-run
  "ROOT's checkpoint, or nil when absent or unreadable."
  [root]
  (let [path (record-path root)]
    (when (fs/exists? path)
      (try (edn/read-string (slurp path))
           (catch Exception _ nil)))))

(defn clear!
  "Drop ROOT's checkpoint. => true when one was removed."
  [root]
  (fs/delete-if-exists (record-path root)))

(defn released
  "{project version} a prior RUN already published. Empty for nil."
  [run]
  (or (:released run) {}))

(defn remaining
  "PLAN with every step already in RELEASED dropped, and waves that empty out
   removed. Wave :index values are preserved so a resumed run reports the same
   wave numbers as the original."
  [plan released]
  (let [done? (comp released :project)
        waves (into []
                    (keep (fn [wave]
                            (let [steps (into [] (remove done?) (:steps wave))]
                              (when (seq steps)
                                (assoc wave :steps steps)))))
                    (:waves plan))]
    (assoc plan :waves waves)))

(defn describe
  "One-line summary of what a prior RUN left behind, or nil when there is
   nothing to resume."
  [run]
  (let [done (released run)]
    (when (seq done)
      (format "resuming: %d project(s) already released (%s)"
              (count done)
              (str/join " " (map (fn [[p v]] (str p "@" v)) (sort done)))))))
