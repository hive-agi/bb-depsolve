(ns bb-depsolve.release.pr-trifecta-test
  "Golden, property and mutation checks for the M4.2 `gh pr create` wrapper,
   driven through a scripted IGhPort: missing executable, unauthenticated gh,
   non-GitHub remote, URL extraction and failure classification."
  (:require [bb-depsolve.release.pr :as pr]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-dsl.result :as r]
            [hive-test.trifecta :as tri]))

(def ^:private url "https://github.com/hive-agi/hive-mcp/pull/42")

(defn- scripted-port
  "IGhPort answering from SCRIPT {:version :auth :create} (each an
   {:exit :out :err} map, :version may carry :missing?) and logging each call's
   argv head into CALLS."
  [script calls]
  (reify pr/IGhPort
    (gh! [_ _dir args]
      (swap! calls conj (vec args))
      (cond
        (= ["--version"] (vec args)) (:version script)
        (= "auth" (first args)) (:auth script)
        :else (:create script)))))

(def ^:private ok-gh {:exit 0 :out "gh version 2" :err ""})

(defn- run-scenario
  "Run `create-pr!` over a scenario {:provider :script :host}; answer
   {:result :calls}."
  [{:keys [provider host script]}]
  (let [calls (atom [])
        result (pr/create-pr! (scripted-port script calls) "/tmp/x"
                              {:provider provider :host host
                               :base "main" :head "minor-release" :tag "v1.2.0"})]
    {:result result :calls @calls}))

(defn- status-of [{:keys [result]}] (some-> result :ok :status))

(defn- sound?
  "The outcome agrees with the scenario: non-GitHub never touches gh, a
   missing gh or failed auth skips without creating, a created PR carries the
   URL gh printed, and an unexplained create failure is an error."
  [{:keys [provider script] :as scenario}]
  (let [{:keys [result calls] :as out} (run-scenario scenario)
        created? (some #(= ["pr" "create"] (take 2 %)) calls)]
    (cond
      (not= :github provider)
      (and (empty? calls) (= :skipped (status-of out))
           (= :non-github-provider (get-in result [:ok :reason])))

      (get-in script [:version :missing?])
      (and (not created?) (= :gh-missing (get-in result [:ok :reason])))

      (pos? (:exit (:auth script)))
      (and (not created?) (= :unauthenticated (get-in result [:ok :reason])))

      (and (zero? (:exit (:create script))) (pr/extract-url (:out (:create script))))
      (and (= :created (status-of out)) (= url (get-in result [:ok :url])))

      (zero? (:exit (:create script))) (r/err? result)

      :else (or (r/err? result) (contains? #{:exists :skipped} (status-of out))))))

(defn- always-create
  "Broken: opens the PR for any provider."
  [scenario]
  (run-scenario (assoc scenario :provider :github)))

(defn- skip-auth-check
  "Broken: ignores a failed auth probe."
  [scenario]
  (run-scenario (assoc-in scenario [:script :auth] {:exit 0 :out "" :err ""})))

(defn- subject
  "Scenario => {:status :reason :url :calls} reduced for golden comparison."
  [scenario]
  (let [{:keys [result calls]} (run-scenario scenario)]
    {:status (or (some-> result :ok :status) :error)
     :reason (or (get-in result [:ok :reason]) (get-in result [:error :kind]))
     :url (get-in result [:ok :url])
     :calls (mapv #(vec (take 2 %)) calls)}))

(def ^:private created {:exit 0 :out (str "Creating pull request\n" url "\n") :err ""})

(defn- scenarios-sound?
  "`sound?` over the subject's scenario input (adapter for the trifecta)."
  [scenario]
  (sound? scenario))

(tri/deftrifecta gh-pr-create-trifecta
  subject
  {:golden-path "test/golden/bb-depsolve/gh-pr-create.edn"
   :cases {:created {:provider :github
                     :script {:version ok-gh :auth ok-gh :create created}}
           :non-github {:provider :gitea
                        :script {:version ok-gh :auth ok-gh :create created}}
           :gh-missing {:provider :github
                        :script {:version {:exit -1 :out "" :err "no gh" :missing? true}
                                 :auth ok-gh :create created}}
           :unauthenticated {:provider :github
                             :script {:version ok-gh
                                      :auth {:exit 1 :out "" :err "You are not logged in"}
                                      :create created}}
           :already-exists {:provider :github
                            :script {:version ok-gh :auth ok-gh
                                     :create {:exit 1 :out ""
                                              :err (str "a pull request for branch already exists:\n" url)}}}
           :create-failed {:provider :github
                           :script {:version ok-gh :auth ok-gh
                                    :create {:exit 1 :out "" :err "GraphQL: boom"}}}}
   :gen (gen/let [provider (gen/elements [:github :gitea :gitlab :other])
                  missing? gen/boolean
                  auth-exit (gen/elements [0 1])
                  create (gen/elements [created
                                        {:exit 0 :out "done\n" :err ""}
                                        {:exit 1 :out "" :err "GraphQL: boom"}
                                        {:exit 1 :out "" :err "gh auth login required"}
                                        {:exit 1 :out "" :err (str "already exists: " url)}])]
          {:provider provider
           :script {:version (if missing?
                               {:exit -1 :out "" :err "" :missing? true}
                               ok-gh)
                    :auth {:exit auth-exit :out "" :err ""}
                    :create create}})
   :pred (fn [scenario] (scenarios-sound? scenario))
   :num-tests 100
   :mutations [["always-create" always-create]
               ["skip-auth-check" skip-auth-check]]})

(deftest extract-url-test
  (testing "last pull URL wins; noise and issue URLs are ignored"
    (is (= url (pr/extract-url (str "Warning: 1 uncommitted change\n" url "\n"))))
    (is (nil? (pr/extract-url "https://github.com/hive-agi/hive-mcp/issues/3")))
    (is (nil? (pr/extract-url nil)))
    (is (nil? (pr/extract-url "see https://github.com/a/b/pull/1 for details")))))

(deftest title-and-args-test
  (testing "default title is release: <tag>"
    (is (= "release: v1.2.0" (pr/pr-title {:tag "v1.2.0"})))
    (is (= "release: v1.2.0" (pr/pr-title {:title "  " :tag "v1.2.0"})))
    (is (= "Custom" (pr/pr-title {:title "Custom" :tag "v1"}))))
  (testing "argv carries base, head, title and body"
    (let [args (pr/pr-args {:base "main" :head "minor-release" :tag "v1.2.0" :body "Notes"})]
      (is (= ["pr" "create" "--base" "main" "--head" "minor-release"
              "--title" "release: v1.2.0" "--body" "Notes"]
             args)))))

(deftest hostname-and-missing-branch-test
  (testing "GitHub Enterprise host is passed to the auth probe"
    (let [calls (atom [])
          port (scripted-port {:version ok-gh :auth ok-gh :create created} calls)]
      (pr/create-pr! port "/tmp/x" {:provider :github :host "ghe.example.org"
                                    :base "main" :head "rel" :tag "v1"})
      (is (= ["auth" "status" "--hostname" "ghe.example.org"] (second @calls)))))
  (testing "a blank head is an error, not a gh call"
    (let [calls (atom [])
          res (pr/create-pr! (scripted-port {} calls) "/tmp/x"
                             {:provider :github :base "main" :head ""})]
      (is (r/err? res))
      (is (empty? @calls)))))
