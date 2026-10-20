(ns bb-depsolve.release.branch
  "Non-default release branch detection (M4.1).

   Compares the branch a release landed on with the repository's default
   branch, so the M4.2 pull-request flow fires only when they differ.

   Pure core: `resolve-default` and `detect` fold plain facts into a verdict.
   The facts come from `git-facts` (read-only git) and, when the caller has
   asked the forge (`gh api repos/OWNER/NAME --jq .default_branch`), from the
   injected :gh-default. Asking the forge stays outside this namespace."
  (:require [bb-depsolve.core.git :as git]
            [clojure.string :as str]))

(def fallback-candidates
  "Branch names a repository may carry as its trunk when neither the forge nor
   origin/HEAD says which. Used only when exactly one of them exists."
  ["main" "master"])

(defn- clean
  "Trimmed S, or nil when S is nil or blank."
  [s]
  (when s
    (let [t (str/trim (str s))]
      (when-not (str/blank? t) t))))

(defn- strip-remote
  "REF without its leading \"REMOTE/\", or nil when REF is blank."
  [remote ref]
  (when-let [r (clean ref)]
    (let [prefix (str remote "/")]
      (if (str/starts-with? r prefix) (subs r (count prefix)) r))))

(defn resolve-default
  "Default branch from FACTS, with the source that named it.

   FACTS: :gh-default       the forge's answer, authoritative when present
          :origin-head      the remote HEAD branch, as read from the clone
          :remote-branches  every branch the remote holds
          :remote           remote name (default \"origin\")
   => {:default branch-or-nil :source :forge|:origin-head|:fallback|:none}

   The fallback names a branch only when exactly one of `fallback-candidates`
   exists remotely; with both (or neither) it refuses to guess."
  [{:keys [gh-default origin-head remote-branches remote] :or {remote "origin"}}]
  (let [forge (strip-remote remote gh-default)
        head (strip-remote remote origin-head)
        present (filterv (set (keep clean remote-branches)) fallback-candidates)]
    (cond
      forge {:default forge :source :forge}
      head {:default head :source :origin-head}
      (= 1 (count present)) {:default (first present) :source :fallback}
      :else {:default nil :source :none})))

(defn detect
  "Verdict on whether the release branch is the repository default.

   FACTS: :current plus everything `resolve-default` reads.
   => {:status :on-default|:off-default|:detached|:unknown
       :current :default :source
       :pr-flow? true only for :off-default (M4.2 trigger)}"
  [facts]
  (let [current (clean (:current facts))
        {:keys [default source]} (resolve-default facts)
        status (cond
                 (nil? current) :detached
                 (nil? default) :unknown
                 (= current default) :on-default
                 :else :off-default)]
    {:status status
     :current current
     :default default
     :source source
     :pr-flow? (= :off-default status)}))

(defn- out
  "Trimmed stdout of the git command in DIR when it succeeded and is not blank."
  [dir & args]
  (let [r (apply git/git dir args)]
    (when (zero? (:exit r))
      (clean (:out r)))))

(defn git-facts
  "Branch facts read from the repository in DIR with read-only git.
   OPTS: :remote (default \"origin\").
   => {:current :origin-head :remote-branches :remote}; :current is nil on a
   detached HEAD, :origin-head nil when the clone never recorded it."
  ([dir] (git-facts dir {}))
  ([dir {:keys [remote] :or {remote "origin"}}]
   (let [ref-prefix (str "refs/remotes/" remote "/")
         listed (out dir "for-each-ref" "--format=%(refname)" (str "refs/remotes/" remote))]
     {:remote remote
      :current (out dir "symbolic-ref" "--quiet" "--short" "HEAD")
      :origin-head (out dir "symbolic-ref" "--quiet" "--short"
                        (str ref-prefix "HEAD"))
      :remote-branches (into []
                             (comp (map str/trim)
                                   (filter #(str/starts-with? % ref-prefix))
                                   (map #(subs % (count ref-prefix)))
                                   (remove #{"HEAD"}))
                             (str/split-lines (or listed "")))})))

(defn detect-in
  "`detect` over the repository in DIR. OPTS: :remote, and :gh-default when
   the caller asked the forge."
  ([dir] (detect-in dir {}))
  ([dir opts]
   (detect (merge (git-facts dir (select-keys opts [:remote]))
                  (select-keys opts [:gh-default])))))
