(ns bb-depsolve.core.git-pins
  "Do the :git/tag and :git/sha of a git coordinate name the same commit?

   tools.deps fetches a git coordinate by its :git/sha, and a short sha that
   names an older commit is accepted as long as it exists. bb-depsolve v0.6.3
   shipped `io.github.hive-agi/hive-dsl {:git/tag \"v0.5.36\" :git/sha \"06f66e9\"}`
   while tag v0.5.36 is e6183cb: the older commit lacked
   hive-dsl.result.taxonomy and the tool crashed at load.

   `pins` walks a deps.edn or bb.edn map (top-level :deps, every alias's
   :deps / :extra-deps / :override-deps / :replace-deps, every bb task's
   :extra-deps) and answers each coordinate holding both a :git/tag and a
   :git/sha. `verdict` compares one pin with what `git ls-remote` printed for
   its tag. Both are pure; `check-files!` is the boundary that runs ls-remote.

   Run it: `bb check-git-pins` (or `clojure -M -m bb-depsolve.core.git-pins`).
   Exit 1 when any pin disagrees or cannot be read."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(def ^:private forge-prefixes
  "Lib group prefix -> clone URL prefix, as tools.deps infers them."
  [["io.github."  "https://github.com/"]
   ["com.github." "https://github.com/"]
   ["io.gitlab."  "https://gitlab.com/"]
   ["com.gitlab." "https://gitlab.com/"]])

(defn git-url
  "The clone URL of LIB's coordinate COORD: its :git/url, else the URL
   tools.deps infers from an io.github / com.github / io.gitlab / com.gitlab
   lib name. nil when neither applies."
  [lib coord]
  (or (:git/url coord)
      (let [group (namespace lib)]
        (some (fn [[prefix url]]
                (when (and group (str/starts-with? group prefix))
                  (str url (subs group (count prefix)) "/" (name lib) ".git")))
              forge-prefixes))))

(defn- dep-maps
  "Every [where deps-map] in a deps.edn or bb.edn map M."
  [m]
  (concat
   [[[:deps] (:deps m)]]
   (for [[alias a] (:aliases m)
         k [:deps :extra-deps :override-deps :replace-deps]
         :when (map? (get a k))]
     [[:aliases alias k] (get a k)])
   (for [[task t] (:tasks m)
         :when (and (map? t) (map? (:extra-deps t)))]
     [[:tasks task :extra-deps] (:extra-deps t)])))

(defn pins
  "Every git coordinate in M (a deps.edn or bb.edn map) that names both a
   :git/tag and a :git/sha, as [{:lib :tag :sha :url :where}]. Pure."
  [m]
  (vec
   (for [[where deps] (dep-maps m)
         [lib coord]  deps
         :when (and (map? coord) (:git/tag coord) (:git/sha coord))]
     {:lib   lib
      :tag   (:git/tag coord)
      :sha   (:git/sha coord)
      :url   (git-url lib coord)
      :where where})))

(defn- ls-row
  "One `git ls-remote` line as [ref sha], nil when blank or malformed."
  [line]
  (let [line (str/trim line)
        i    (or (str/index-of line "\t") (str/index-of line " "))]
    (when i
      (let [sha (subs line 0 i) ref (str/trim (subs line i))]
        (when (and (seq sha) (seq ref)) [ref sha])))))

(defn tag-commit
  "The commit sha `git ls-remote <url> refs/tags/TAG refs/tags/TAG^{}`
   printed for TAG: the peeled `^{}` line of an annotated tag, else the
   lightweight tag's own line. nil when the tag is absent. Pure."
  [tag out]
  (let [rows (into {} (keep ls-row) (str/split-lines (or out "")))]
    (or (get rows (str "refs/tags/" tag "^{}"))
        (get rows (str "refs/tags/" tag)))))

(defn verdict
  "PIN with :status :ok | :mismatch | :missing-tag | :unreadable, and
   :tag-sha when the tag was found. LS is {:exit :out :err} of ls-remote, or
   nil when PIN has no URL to ask. The pinned sha may be short: it agrees when
   the tag's commit starts with it. Pure."
  [{:keys [sha tag] :as pin} ls]
  (cond
    (nil? ls)           (assoc pin :status :unreadable :detail "no git URL for this lib")
    (not= 0 (:exit ls)) (assoc pin :status :unreadable :detail (str/trim (str (:err ls))))
    :else
    (if-let [tag-sha (tag-commit tag (:out ls))]
      (assoc pin :tag-sha tag-sha
             :status (if (str/starts-with? tag-sha (str/lower-case (str sha))) :ok :mismatch))
      (assoc pin :status :missing-tag))))

(defn describe
  "One report line per verdict. Pure."
  [{:keys [lib tag sha tag-sha status detail where]}]
  (str (case status
         :ok          "ok        "
         :mismatch    "MISMATCH  "
         :missing-tag "NO-TAG    "
         :unreadable  "UNREAD    ")
       lib " " tag " " sha
       (case status
         :mismatch    (str " but the tag is " tag-sha "; pin :git/sha \"" (subs tag-sha 0 7) "\"")
         :missing-tag " but the remote has no such tag"
         :unreadable  (str " (" detail ")")
         "")
       "  at " (pr-str where)))

(defn ls-remote
  "Boundary: `git ls-remote URL refs/tags/TAG refs/tags/TAG^{}`."
  [url tag]
  (sh/sh "git" "ls-remote" url (str "refs/tags/" tag) (str "refs/tags/" tag "^{}")))

(defn check-files!
  "Verdicts for every git pin in FILES (paths to deps.edn / bb.edn; a missing
   file is skipped). LS-FN is (url tag) -> {:exit :out :err}, default
   `ls-remote`. Each [url tag] is asked once."
  ([files] (check-files! files ls-remote))
  ([files ls-fn]
   (let [ask (memoize ls-fn)]
     (vec (for [f     files
                :when (.exists (io/file f))
                pin   (pins (edn/read-string (slurp f)))]
            (-> (verdict pin (when (:url pin) (ask (:url pin) (:tag pin))))
                (update :where #(into [f] %))))))))

(defn -main
  "Check the git pins of deps.edn and bb.edn (or the files named as args).
   Exit 1 when any disagrees or cannot be read."
  [& args]
  (let [vs  (check-files! (or (seq args) ["deps.edn" "bb.edn"]))
        bad (remove #(= :ok (:status %)) vs)]
    (doseq [v vs] (println (describe v)))
    (println (str (count vs) " git pin(s) checked, " (count bad) " bad"))
    (shutdown-agents)
    (System/exit (if (seq bad) 1 0))))
