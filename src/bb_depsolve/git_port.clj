(ns bb-depsolve.git-port
  "IReleasePort over the working tree."
  (:require [babashka.fs :as fs]
            [bb-depsolve.collect :as collect]
            [bb-depsolve.core :as core]
            [bb-depsolve.port :as port]
            [bb-depsolve.version :as v]
            [clojure.string :as str]
            [hive-dsl.result :as r]))

(def pin-commit-message "chore(deps): sync internal pins")

(defn tag-sha
  "Short sha the remote serves for TAG of LIB, or nil."
  [lib tag]
  (when-let [{:keys [org repo]} (v/parse-github-lib lib)]
    (let [result (core/resolve-remote-tags org repo)]
      (when (r/ok? result)
        (some #(when (= tag (:tag %)) (or (:sha-short %) (:sha %)))
              (:ok result))))))

(defn apply-pin
  "CONTENT with PIN's coordinate rewritten to its :to."
  [content {:keys [lib coord to sha]}]
  (case coord
    :git (if sha (v/update-git-dep content lib to sha) content)
    :mvn (v/update-mvn-dep content lib to)
    content))

(defn resolve-shas
  "PINS with every :git pin carrying the sha of its target tag.
   => Result; errs when a target tag resolves to no sha."
  [pins]
  (let [resolved (mapv #(cond-> %
                          (= :git (:coord %)) (assoc :sha (tag-sha (:lib %) (:to %))))
                       pins)
        missing (filter #(and (= :git (:coord %)) (nil? (:sha %))) resolved)]
    (if (seq missing)
      (r/err {:kind :git-port/unresolved-tag
              :pins (mapv #(select-keys % [:lib :to]) missing)})
      (r/ok resolved))))

(defn- git!
  [write? dir args]
  (if-not write?
    (r/ok {:exit 0 :rehearsed args})
    (let [{:keys [exit err] :as result} (apply core/git dir args)]
      (if (zero? (long exit))
        (r/ok result)
        (r/err {:kind :git-port/git-failed
                :dir (str dir)
                :args (vec args)
                :message (str/trim (or err ""))})))))

(defn- git-seq!
  "Run COMMANDS in DIR in order, stopping at the first failure."
  [write? dir commands]
  (reduce (fn [_ args]
            (let [result (git! write? dir args)]
              (if (r/ok? result) result (reduced result))))
          (r/ok nil)
          commands))

(defn- write-pins!
  "Apply PINS to their files. => paths whose content changed."
  [write? pins]
  (reduce (fn [acc [path file-pins]]
            (let [content (slurp (str path))
                  updated (reduce apply-pin content file-pins)]
              (if (= content updated)
                acc
                (do (when write? (spit (str path) updated))
                    (conj acc (str path))))))
          []
          (group-by :path pins)))

(defn- version-files
  "Every VERSION file under DIR, the root one first."
  [dir]
  (let [root-file (str (fs/path dir "VERSION"))]
    (into [root-file]
          (->> (fs/glob dir "**/VERSION") (map str) (remove #{root-file})))))

(defn- release-pinned!
  [write? remote dir project version]
  (if-not version
    (r/err {:kind :git-port/no-target-version :project project})
    (let [tag (str "v" version)
          files (version-files dir)]
      (when write?
        (doseq [f files] (spit f (str version "\n"))))
      (r/let-ok [_ (git-seq! write? dir
                             (concat (for [f files]
                                       ["add" (str (fs/relativize dir f))])
                                     [["commit" "-m" (str "release: " tag)]
                                      ["tag" tag]
                                      ["push" remote "HEAD"]
                                      ["push" remote tag]]))]
        (r/ok {:project project :release-mode :pinned :version version :tag tag})))))

(defn- release-rolling!
  [write? remote dir project]
  (r/let-ok [_ (git-seq! write? dir [["push" remote "HEAD"]])]
    (let [version (collect/project-version dir (collect/read-version-config dir) :rolling)]
      (if version
        (r/ok {:project project :release-mode :rolling :version version :tag nil})
        (r/err {:kind :git-port/no-minted-version :project project})))))

(defrecord GitPort [opts]
  port/IReleasePort
  (sync-pins! [_ step]
    (let [{:keys [project dir pin-updates]} step
          {applied true skipped false} (group-by (comp some? :to) pin-updates)
          write? (:write? opts)]
      (r/let-ok [pins (resolve-shas (vec applied))]
        (let [written (write-pins! write? pins)]
          (r/let-ok [_ (if (seq written)
                         (git-seq! write? dir
                                   (concat (for [f written]
                                             ["add" (str (fs/relativize dir f))])
                                           [["commit" "-m" pin-commit-message]]))
                         (r/ok :nothing-to-commit))]
            (r/ok {:project project
                   :paths (into (sorted-set) written)
                   :applied (vec pins)
                   :skipped (vec skipped)}))))))

  (release! [_ step]
    (let [{:keys [project dir release-mode next-version]} step
          write? (:write? opts)
          remote (:remote opts)]
      (case release-mode
        :pinned (release-pinned! write? remote dir project next-version)
        :rolling (release-rolling! write? remote dir project)
        (r/err {:kind :git-port/unknown-release-mode
                :project project
                :release-mode release-mode})))))

(defn git-port
  "IReleasePort writing to the working tree.

   OPTS: :write?  perform the effects (default true); false rehearses
         :remote  push remote (default \"origin\")"
  ([] (git-port {}))
  ([opts] (->GitPort (merge {:write? true :remote "origin"} opts))))
