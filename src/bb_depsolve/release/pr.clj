(ns bb-depsolve.release.pr
  "Pull-request wrapper over the GitHub CLI (M4.2).

   Opens `gh pr create --base <default> --head <current>` for a release that
   landed on a non-default branch (the M4.1 verdict in
   `bb-depsolve.release.branch`), and answers the PR URL for the cascade
   output.

   Pure core: `pr-title`, `pr-body`, `pr-args`, `extract-url` and
   `classify-failure` fold plain data. GitHub CLI effects cross `IGhPort`;
   read-only git, VERSION, template and checkpoint inputs enrich the default
   M4.3 body at the `open-release-pr!` boundary. The real `gh` process adapter
   (`process-port`) stays at the edge.

   Degradation is a value, not an exception: a missing `gh`, an
   unauthenticated `gh` and a non-GitHub remote all answer
   `{:status :skipped :reason ... :message ...}` so the cascade warns and
   continues. Only a `gh pr create` that really failed answers an error."
  (:require [babashka.process :as proc]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.release.branch :as branch]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [bb-depsolve.release.pr-body :as body]
            [bb-depsolve.release.resume :as resume]
            [bb-depsolve.core.remote :as remote]
            [clojure.edn :as edn]))

(defprotocol IGhPort
  "The GitHub CLI as an effect. One method, so a stub is a one-liner."
  (gh! [this dir args]
    "Run `gh ARGS...` in DIR.
     => {:exit n :out string :err string}, plus :missing? true when the
     executable could not be started (exit is -1 then)."))

(defn default-title
  "Title used when the caller supplies none."
  [tag]
  (str "release: " (or tag "unreleased")))

(defn pr-title
  "TITLE when it is not blank, else `release: <tag>`. Pure."
  [{:keys [title tag]}]
  (if (and title (not (str/blank? (str title))))
    (str/trim (str title))
    (default-title tag)))

(defn pr-body
  "Non-blank :body verbatim, else the pure M4.3 generator over OPTS."
  [{:keys [body] :as opts}]
  (if (and body (not (str/blank? (str body))))
    (str body)
    (body/render opts)))

(defn pr-args
  "The `gh` argv for the pull request. Pure.
   OPTS: :base :head :title :body :tag."
  [{:keys [base head] :as opts}]
  ["pr" "create"
   "--base" (str base)
   "--head" (str head)
   "--title" (pr-title opts)
   "--body" (pr-body opts)])

(defn- pull-url?
  "True when LINE is an http(s) URL that names a pull request."
  [line]
  (and (or (str/starts-with? line "https://") (str/starts-with? line "http://"))
       (str/includes? line "/pull/")
       (not (str/includes? line " "))))

(defn extract-url
  "The pull-request URL in TEXT (the last such line), or nil. Pure."
  [text]
  (->> (str/split-lines (or text ""))
       (map str/trim)
       (filter pull-url?)
       last))

(defn classify-failure
  "Why a failed `gh pr create` failed, from its result. Pure.
   => :already-exists | :unauthenticated | :failed"
  [{:keys [out err]}]
  (let [text (str/lower-case (str out "\n" err))]
    (cond
      (and (str/includes? text "already exists") (extract-url text)) :already-exists
      (or (str/includes? text "gh auth login")
          (str/includes? text "not logged in")
          (str/includes? text "authentication required")
          (str/includes? text "bad credentials")) :unauthenticated
      :else :failed)))

(defn- skipped
  "A graceful-degradation outcome: the cascade warns and continues."
  [reason message]
  (r/ok {:status :skipped :reason reason :message message :url nil}))

(defn- hostname-args
  "`--hostname HOST` for a GitHub Enterprise HOST, nothing for github.com."
  [host]
  (if (and host (not= "github.com" host)) ["--hostname" host] []))

(defn create-pr!
  "Open a pull request through PORT. Never throws.
   OPTS: :provider  forge keyword of the remote (anything but :github skips)
         :host      remote hostname, for GitHub Enterprise auth checks
         :base :head :tag :title :body   see `pr-args`
   => Result of {:status :created|:exists|:skipped :url :reason :message};
      :skipped carries :reason :non-github-provider | :gh-missing |
      :unauthenticated. A `gh pr create` that fails for any other reason is an
      error {:kind :pr/create-failed}."
  [port dir {:keys [provider host base head] :as opts}]
  (cond
    (not= :github provider)
    (skipped :non-github-provider
             (str "remote provider is " (if provider (name provider) "unknown")
                  ", not github: pull request not opened"))

    (or (str/blank? (str base)) (str/blank? (str head)))
    (r/err {:kind :pr/missing-branch :base base :head head})

    :else
    (let [probe (gh! port dir ["--version"])]
      (if (:missing? probe)
        (skipped :gh-missing "gh is not on PATH: open the pull request by hand")
        (let [auth (gh! port dir (into ["auth" "status"] (hostname-args host)))]
          (if-not (zero? (long (:exit auth)))
            (skipped :unauthenticated "gh is not authenticated: run `gh auth login`")
            (let [res (gh! port dir (pr-args opts))
                  url (extract-url (:out res))]
              (cond
                (and (zero? (long (:exit res))) url)
                (r/ok {:status :created :url url})

                (zero? (long (:exit res)))
                (r/err {:kind :pr/no-url :out (:out res)})

                :else
                (case (classify-failure res)
                  :already-exists (r/ok {:status :exists
                                         :url (extract-url (str (:out res) "\n" (:err res)))})
                  :unauthenticated (skipped :unauthenticated
                                            "gh is not authenticated: run `gh auth login`")
                  (r/err {:kind :pr/create-failed
                          :exit (:exit res)
                          :message (str/trim (str (:err res)))}))))))))))

(defn process-port
  "IGhPort running the real `gh` executable. GH_PROMPT_DISABLED keeps it from
   waiting on a terminal that is not there."
  []
  (reify IGhPort
    (gh! [_ dir args]
      (try
        (let [res (proc/sh {:dir (str dir)
                            :extra-env {"GH_PROMPT_DISABLED" "1"}}
                           (into ["gh"] args))]
          (select-keys res [:exit :out :err]))
        (catch Exception e
          {:exit -1 :out "" :err (ex-message e) :missing? true})))))

(defn- git-text
  "Trimmed stdout on success, nil on failure. An empty log stays empty."
  [dir & args]
  (let [res (apply git/git dir args)]
    (when (zero? (:exit res))
      (str/trim (:out res)))))

(defn- read-text
  [path]
  (try
    (not-empty (str/trim (slurp path)))
    (catch Exception _ nil)))

(defn- read-template
  [root]
  (when-let [text (read-text (remote/config-path root))]
    (try
      (:pr-template (edn/read-string text))
      (catch Exception _ nil))))

(defn- with-body-facts
  "Read-only inputs for the pure generator. Caller-supplied values win,
   including an in-memory :checkpoint before the cascade clears it on disk."
  [dir {:keys [root remote base head] :as opts}]
  (let [root (or root dir)
        base-ref (delay
                   (let [ref (str "refs/remotes/" (or remote "origin") "/" base)]
                     (if (git-text dir "rev-parse" "--verify" (str ref "^{commit}"))
                       ref
                       base)))
        readers {:git-log #(git-text dir "log" "--format=%s" (str @base-ref ".." head) "--")
                 :version #(read-text (str dir "/VERSION"))
                 :previous-version #(not-empty (git-text dir "show" (str @base-ref ":VERSION")))
                 :pr-template #(read-template root)
                 :checkpoint #(resume/load-run root)}]
    (reduce-kv (fn [facts k read-value]
                 (if (contains? facts k) facts (assoc facts k (read-value))))
               opts readers)))

(defn open-release-pr!
  "Open the release pull request for the repository in DIR when its current
   branch is not the default (M4.1 `branch/detect-in`). Skips on-default,
   detached and unknown branches.
   OPTS: :port (default `process-port`), :remote, :gh-default, :overrides
         (host->provider), :tag, :title, :body.
   Without a non-blank :body, read base..head commit subjects and VERSION from
   git, DIR/VERSION, and ROOT's .bb-depsolve.edn :pr-template and M1.3
   checkpoint (ROOT defaults to DIR). :git-log, :version, :previous-version,
   :pr-template and :checkpoint may instead be supplied as plain facts.
   => Result as `create-pr!`, or {:status :skipped :reason :not-off-default
      :verdict ...}."
  [dir {:keys [port overrides] :as opts}]
  (let [verdict (branch/detect-in dir (select-keys opts [:remote :gh-default]))]
    (if-not (:pr-flow? verdict)
      (r/ok {:status :skipped :reason :not-off-default :url nil
             :message (str "release branch check: " (name (:status verdict)))
             :verdict verdict})
      (let [url (git/git-first-remote-url dir)
            host (git/remote-host url)
            opts (assoc opts
                        :provider (git/remote-provider url (or overrides {}))
                        :host host
                        :base (:default verdict)
                        :head (:current verdict))
            opts (if (and (= :github (:provider opts))
                          (str/blank? (str (:body opts))))
                   (let [facts (with-body-facts dir opts)]
                     (assoc facts :body (pr-body facts)))
                   opts)]
        (create-pr! (or port (process-port)) dir opts)))))
