(ns bb-depsolve.release.pr-body-integration-test
  "The existing PR wrapper reads body facts; gh stays a scripted port.
   Scratch files stay under this checkout and are removed after every test."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.git :as git]
            [bb-depsolve.release.branch :as branch]
            [bb-depsolve.release.pr :as pr]
            [bb-depsolve.release.pr-body :as body]
            [bb-depsolve.release.resume :as resume]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private url "https://github.com/hive-agi/example/pull/42")
(def ^:private verdict {:pr-flow? true :default "main" :current "minor-release"})

(defn- scripted-port
  [calls]
  (reify pr/IGhPort
    (gh! [_ dir args]
      (swap! calls conj [dir args])
      (case (first args)
        "--version" {:exit 0 :out "gh version 2" :err ""}
        "auth" {:exit 0 :out "" :err ""}
        "pr" {:exit 0 :out (str url "\n") :err ""}
        (throw (ex-info "Unexpected gh call" {:args args}))))))

(defn- sent-body
  [calls]
  (let [args (second (last calls))]
    (second (drop-while #(not= "--body" %) args))))

(defn- with-root
  [f]
  (let [root (str (fs/create-temp-dir {:dir "." :prefix ".pr-body-test-"}))]
    (try (f root)
         (finally (fs/delete-tree root)))))

(defn- github-url [_dir] "git@github.com:hive-agi/example.git")

(deftest default-body-reads-release-facts-test
  (with-root
    (fn [root]
      (let [dir (str (fs/path root "consumer"))
            calls (atom [])
            git-calls (atom [])
            checkpoint {:status :running :released {"consumer" "1.2.0"}
                        :waves [{:index 0
                                 :steps [{:project "consumer" :status :released
                                          :pin-updates [{:dep "dep" :lib 'io.github.hive-agi/dep
                                                         :from "0.1.0" :to "0.1.1"}]}]}]}
            log "feat: automate release notes\nfix: keep aliases\n"
            template {:test-plan "- [ ] Verify the staged release."}]
        (fs/create-dirs dir)
        (spit (str (fs/path dir "VERSION")) "1.2.0\n")
        (spit (str (fs/path root ".bb-depsolve.edn")) (pr-str {:pr-template template}))
        (resume/save! root checkpoint)
        (with-redefs [branch/detect-in (fn [_ _] verdict)
                      git/git-first-remote-url github-url
                      git/git (fn [dir & args]
                                (swap! git-calls conj [dir (vec args)])
                                (case (first args)
                                  "rev-parse" {:exit 0 :out "abc1234\n"}
                                  "log" {:exit 0 :out log}
                                  "show" {:exit 0 :out "1.1.0\n"}
                                  (throw (ex-info "Unexpected git call" {:args args}))))]
          (is (= {:ok {:status :created :url url}}
                 (pr/open-release-pr! dir {:root root :tag "v1.2.0"
                                          :port (scripted-port calls)}))))
        (is (= (body/render {:tag "v1.2.0" :version "1.2.0" :previous-version "1.1.0"
                              :base "main" :head "minor-release" :git-log log
                              :checkpoint checkpoint :pr-template template})
               (sent-body @calls)))
        (is (some #{[dir ["log" "--format=%s" "refs/remotes/origin/main..minor-release" "--"]]}
                  @git-calls))
        (is (some #{[dir ["show" "refs/remotes/origin/main:VERSION"]]} @git-calls))
        (is (= ["pr" "create" "--base" "main" "--head" "minor-release"]
               (take 6 (second (last @calls)))))))))

(deftest caller-facts-can-replace-every-read-test
  (let [calls (atom [])
        opts {:tag "v2.0.0" :git-log "fix: injected log" :version "2.0.0"
              :previous-version "1.0.0" :checkpoint nil
              :pr-template {:test-plan "- [ ] Injected verification."}}]
    (with-redefs [branch/detect-in (fn [_ _] verdict)
                  git/git-first-remote-url github-url
                  git/git (fn [& _] (throw (ex-info "Unexpected git read" {})))
                  resume/load-run (fn [_] (throw (ex-info "Unexpected checkpoint read" {})))]
      (pr/open-release-pr! "." (assoc opts :port (scripted-port calls))))
    (is (= (body/render (assoc opts :base "main" :head "minor-release"))
           (sent-body @calls)))))

(deftest explicit-body-is-preserved-without-enrichment-test
  (let [calls (atom [])
        explicit " Custom release notes\n- Keep this formatting.\n"]
    (with-redefs [branch/detect-in (fn [_ _] verdict)
                  git/git-first-remote-url github-url
                  git/git (fn [& _] (throw (ex-info "Unexpected git read" {})))
                  resume/load-run (fn [_] (throw (ex-info "Unexpected checkpoint read" {})))]
      (pr/open-release-pr! "." {:body explicit :port (scripted-port calls)}))
    (is (= explicit (sent-body @calls)))
    (is (= explicit (pr/pr-body {:body explicit})))
    (is (= (body/render {:base "main" :head "release" :body "  "})
           (pr/pr-body {:base "main" :head "release" :body "  "})))))

(deftest missing-facts-and-unreadable-files-degrade-test
  (doseq [config [nil "{:pr-template"]]
    (with-root
      (fn [root]
        (let [calls (atom [])
              git-calls (atom [])]
          (when config
            (spit (str (fs/path root ".bb-depsolve.edn")) config)
            (fs/create-dirs (fs/path root ".bb-depsolve"))
            (spit (resume/record-path root) "{:waves"))
          (with-redefs [branch/detect-in (fn [_ _] verdict)
                        git/git-first-remote-url github-url
                        git/git (fn [_ & args]
                                  (swap! git-calls conj (vec args))
                                  {:exit 1 :out "" :err "no ref"})]
            (is (= :created (get-in (pr/open-release-pr!
                                    root {:port (scripted-port calls)})
                                   [:ok :status]))))
          (is (= (body/render {:base "main" :head "minor-release"}) (sent-body @calls)))
          (is (some #{["log" "--format=%s" "main..minor-release" "--"]} @git-calls)))))))

(deftest non-origin-remote-and-default-root-test
  (with-root
    (fn [root]
      (let [calls (atom [])
            git-calls (atom [])]
        (spit (str (fs/path root ".bb-depsolve.edn"))
              (pr-str {:pr-template {:test-plan "- [ ] Root defaults to DIR."}}))
        (with-redefs [branch/detect-in (fn [_ opts]
                                        (is (= {:remote "upstream"} opts))
                                        verdict)
                      git/git-first-remote-url github-url
                      git/git (fn [_ & args]
                                (swap! git-calls conj (vec args))
                                {:exit 0 :out ""})]
          (pr/open-release-pr! root {:remote "upstream" :port (scripted-port calls)}))
        (is (some #{["log" "--format=%s" "refs/remotes/upstream/main..minor-release" "--"]}
                  @git-calls))
        (is (str/includes? (sent-body @calls) "- [ ] Root defaults to DIR."))))))

(deftest skipped-prs-do-not-collect-body-facts-test
  (testing "gitea is skipped without gh or body inputs"
    (let [calls (atom [])]
      (with-redefs [branch/detect-in (fn [_ _] verdict)
                    git/git-first-remote-url (fn [_] "git@gitea.example.org:org/repo.git")
                    git/git (fn [& _] (throw (ex-info "Unexpected git read" {})))]
        (is (= :non-github-provider
               (get-in (pr/open-release-pr! "." {:port (scripted-port calls)}) [:ok :reason]))))
      (is (empty? @calls))))
  (testing "a default-branch release never asks for the remote or body"
    (with-redefs [branch/detect-in (fn [_ _] {:pr-flow? false :status :on-default})
                  git/git-first-remote-url (fn [_] (throw (ex-info "Unexpected remote read" {})))]
      (is (= :not-off-default (get-in (pr/open-release-pr! "." {}) [:ok :reason]))))))
