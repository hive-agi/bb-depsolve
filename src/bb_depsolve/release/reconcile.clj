(ns bb-depsolve.release.reconcile
  "Reconcile VERSION files that lag the versions already evidenced elsewhere.

   A project's VERSION can fall behind what it has actually released: its own
   git tags run ahead, or its consumers already pin a higher version. The
   planner refuses to plan a downgrade and reports the drift, but the file
   stays wrong until something rewrites it.

   drift {:project :declared :highest :from-tags :from-pins}"
  (:require [babashka.fs :as fs]
            [bb-depsolve.cascade.plan :as cas]
            [bb-depsolve.core.resolve :as resolve]
            [bb-depsolve.version.api :as v]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [babashka.process :as proc]))

(defn highest-version
  "Highest of VERSIONS by semver order, ignoring nils. Nil when none."
  [versions]
  (last (sort v/version-compare (remove nil? versions))))

(defn drift
  "Drift for PROJECT, or nil when its DECLARED version is already the highest.

   Evidence is the declared version, the highest version consumers pin (via
   the graph), and TAG-VERSION — the project's own latest git tag."
  [graph project declared tag-version]
  (let [{:keys [observed]} (cas/effective-version graph project declared)
        highest (highest-version [declared observed tag-version])]
    (when (and highest declared (v/version-newer? declared highest))
      {:project project
       :declared declared
       :highest highest
       :from-tags tag-version
       :from-pins observed})))

(defn read-declared
  "Version string in DIR's VERSION file, or nil when absent or unparseable."
  [dir]
  (let [path (fs/path dir "VERSION")]
    (when (fs/exists? path)
      (let [s (str/trim (slurp (str path)))]
        (when (v/parse-semver s) s)))))

(defn latest-tag-version
  "Maven-form version of DIR's latest local semver tag, or nil."
  [dir]
  (let [result (resolve/resolve-local-tags dir)]
    (when (r/ok? result)
      (some-> (v/latest-tag (:ok result)) :tag v/tag->mvn-version))))

(defn survey
  "Every drifting project under GRAPH, given DIR-OF (fn [project] -> dir).
   Sorted by project. Pure apart from DIR-OF's reads."
  [graph dir-of]
  (into []
        (keep (fn [project]
                (let [dir (dir-of project)]
                  (when-let [declared (read-declared dir)]
                    (drift graph project declared (latest-tag-version dir))))))
        (sort (keys (:nodes graph)))))

(defn apply-drift!
  "Rewrite every VERSION file under DIR to the drift's :highest, skipping a
   file that already holds it: a rewrite that leaves a no-op diff looks like
   work in progress to every later tool (and collides with a release commit).
   => the paths actually changed."
  [dir {:keys [highest]}]
  (let [content (str highest "\n")
        paths (cons (str (fs/path dir "VERSION"))
                    (map str (fs/glob dir "**/VERSION")))]
    (into []
          (keep (fn [p]
                  (when (and (fs/exists? p) (not= content (slurp p)))
                    (spit p content)
                    p)))
          (distinct paths))))

(defn partition-upstream
  "Split DRIFTS by what each project's upstream branch already declares.
   UPSTREAM is {project version-or-nil}. A drift whose upstream VERSION is
   already at (or past) :highest is a :pull: the fix is to take upstream, and
   rewriting the file locally only produces an uncommitted edit that collides
   with the next release commit. Every other drift is a :rewrite.
   => {:pull [drift+:upstream ...] :rewrite [drift ...]}"
  [drifts upstream]
  (reduce (fn [acc {:keys [project highest] :as d}]
            (let [up (get upstream project)]
              (if (and up (v/parse-semver up) (not (v/version-newer? up highest)))
                (update acc :pull conj (assoc d :upstream up))
                (update acc :rewrite conj d))))
          {:pull [] :rewrite []}
          drifts))

(defn upstream-version
  "VERSION declared on DIR's upstream branch (@{u}), or nil when there is no
   upstream, no VERSION there, or it does not parse. Boundary: runs git."
  [dir]
  (let [{:keys [exit out]} (proc/sh ["git" "-C" (str dir) "show" "@{u}:VERSION"])]
    (when (zero? exit)
      (let [s (str/trim out)]
        (when (v/parse-semver s) s)))))
