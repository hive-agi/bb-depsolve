(ns bb-depsolve.release.release-pr-test
  "--release-pr (M4): after a release is pushed, the cascade and bump open the
   release pull request through an injected forge port. The forge here is a
   recording stub fn, so no gh, no network and no with-redefs."
  (:require [babashka.fs :as fs]
            [babashka.process :as proc]
            [bb-depsolve.core.bump :as bump]
            [bb-depsolve.release.run :as run]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(def ^:private pr-url "https://github.com/hive-agi/example/pull/7")

(defn- forge-stub
  "Forge port stand-in, shaped like `pr/open-release-pr!`: records each
   [dir opts] it is asked for and answers (RESPOND dir opts)."
  ([calls] (forge-stub calls (fn [_ _] (r/ok {:status :created :url pr-url}))))
  ([calls respond]
   (fn [dir opts]
     (swap! calls conj [dir opts])
     (respond dir opts))))

;; =============================================================================
;; Cascade: open-release-prs!
;; =============================================================================

(def ^:private plan
  {:waves [{:index 0 :steps [{:project "weave" :dir "/ws/weave"}
                             {:project "dsl" :dir "/ws/dsl"}]}
           {:index 1 :steps [{:project "mcp" :dir "/ws/mcp"}]}]})

(def ^:private weave-pin
  {:dep "weave" :lib 'io.github.hive-agi/hive-weave :coord :mvn
   :path "/ws/mcp/deps.edn" :from "0.4.1" :to "0.4.2"})

(def ^:private checkpoint
  "An earlier invocation's aborted wave, then this run's waves: dsl failed
   first and was released on resume; mcp is a rolling release with no tag;
   ghost is released but absent from the plan."
  {:status :complete
   :released {"weave" "0.4.2" "dsl" "0.5.38" "mcp" "0.1.90"}
   :waves [{:index 0 :steps [{:project "weave" :status :released :version "0.4.2"
                              :tag "v0.4.2" :pin-updates []}
                             {:project "dsl" :status :release-failed :pin-updates []}]}
           {:index 0 :steps [{:project "dsl" :status :released :version "0.5.38"
                              :tag "v0.5.38" :pin-updates []}]}
           {:index 1 :steps [{:project "mcp" :status :released :version "0.1.90"
                              :tag nil :pin-updates [weave-pin]}
                             {:project "ghost" :status :released :version "9.9.9"
                              :tag "v9.9.9" :pin-updates []}]}]})

(deftest open-release-prs-asks-the-forge-once-per-released-project-test
  (let [calls (atom [])
        results (atom nil)
        out (with-out-str
              (reset! results (run/open-release-prs! (forge-stub calls) "/ws" plan checkpoint)))
        created (r/ok {:status :created :url pr-url})]
    (is (= [{:project "weave" :dir "/ws/weave" :result created}
            {:project "dsl" :dir "/ws/dsl" :result created}
            {:project "mcp" :dir "/ws/mcp" :result created}]
           @results))
    (testing "release order, released steps only, each at its planned dir"
      (is (= ["/ws/weave" "/ws/dsl" "/ws/mcp"] (mapv first @calls))))
    (testing "a rolling release is titled by its version, a pinned one by its tag"
      (is (= ["v0.4.2" "v0.5.38" "0.1.90"] (mapv (comp :tag second) @calls))))
    (testing "every PR body reads the workspace config and the in-memory checkpoint"
      (is (every? #(= "/ws" (:root (second %))) @calls))
      (is (every? #(= checkpoint (:checkpoint (second %))) @calls)))
    (is (= 3 (count (re-seq #"release PR opened:" out))))
    (is (str/includes? out pr-url))))

(deftest open-release-prs-degrade-to-warnings-test
  (let [answers {"/ws/weave" (fn [] (r/ok {:status :skipped :reason :gh-missing :url nil
                                           :message "gh is not on PATH: open the pull request by hand"}))
                 "/ws/dsl" (fn [] (r/err {:kind :pr/create-failed :exit 1 :message "HTTP 422"}))
                 "/ws/mcp" (fn [] (throw (ex-info "detect-in blew up" {})))}
        calls (atom [])
        results (atom nil)
        out (with-out-str
              (reset! results (run/open-release-prs!
                               (forge-stub calls (fn [dir _] ((answers dir))))
                               "/ws" plan checkpoint)))]
    (testing "nothing throws: every project is still asked for"
      (is (= 3 (count @calls)))
      (is (= :gh-missing (get-in @results [0 :result :ok :reason])))
      (is (= :pr/create-failed (get-in @results [1 :result :error :kind])))
      (is (= {:kind :pr/threw :message "detect-in blew up"} (get-in @results [2 :result :error]))))
    (testing "each outcome is reported"
      (is (str/includes? out "release PR skipped: gh is not on PATH"))
      (is (str/includes? out "release PR failed: HTTP 422"))
      (is (str/includes? out "release PR failed: detect-in blew up")))))

(deftest open-release-prs-reports-a-default-branch-release-quietly-test
  (let [out (with-out-str
              (run/open-release-prs!
               (forge-stub (atom []) (fn [_ _] (r/ok {:status :skipped :reason :not-off-default
                                                      :url nil :message "release branch check: on-default"})))
               "/ws" {:waves [{:steps [{:project "weave" :dir "/ws/weave"}]}]}
               {:waves [{:steps [{:project "weave" :status :released :version "0.4.2" :tag "v0.4.2"}]}]}))]
    (is (str/includes? out "no release PR (release branch check: on-default)"))
    (is (not (str/includes? out "skipped")))))

(deftest open-release-prs-without-releases-asks-nothing-test
  (let [calls (atom [])]
    (is (= [] (run/open-release-prs! (forge-stub calls) "/ws" plan {:waves []})))
    (is (= [] (run/open-release-prs! (forge-stub calls) "/ws" plan nil)))
    (is (= [] @calls))))

;; =============================================================================
;; bump --release-pr, against a real temp remote
;; =============================================================================

(defn- sh!
  [dir & args]
  (let [res (proc/sh (into ["git" "-C" (str dir)] args))]
    (when-not (zero? (:exit res))
      (throw (ex-info "fixture git failed" {:args args :res res})))
    res))

(defn- with-release-repo
  "Call (F work remote) with a clone at VERSION 0.4.2 whose origin is a bare
   repository, so the release push lands. With PROTECTED?, origin is a non-bare
   repository with main checked out instead, which refuses the branch push but
   accepts tags: the GH006 shape."
  ([f] (with-release-repo false f))
  ([protected? f]
   (let [root (str (fs/create-temp-dir {:prefix "bb-depsolve-release-pr"}))
         remote (str (fs/path root "remote"))
         work (str (fs/path root "work"))
         identity! (fn [dir]
                     (sh! dir "config" "user.email" "t@t")
                     (sh! dir "config" "user.name" "t")
                     (sh! dir "config" "commit.gpgsign" "false")
                     (sh! dir "config" "tag.gpgsign" "false"))]
     (try
       (if protected?
         (do (sh! root "init" "-q" "-b" "main" remote)
             (identity! remote)
             (spit (str (fs/path remote "VERSION")) "0.4.2\n")
             (sh! remote "add" "VERSION")
             (sh! remote "commit" "-q" "-m" "init")
             (sh! root "clone" "-q" remote work)
             (identity! work))
         (do (sh! root "init" "-q" "--bare" "-b" "main" remote)
             (sh! root "init" "-q" "-b" "main" work)
             (identity! work)
             (spit (str (fs/path work "VERSION")) "0.4.2\n")
             (sh! work "add" "VERSION")
             (sh! work "commit" "-q" "-m" "init")
             (sh! work "remote" "add" "origin" remote)
             (sh! work "push" "-q" "-u" "origin" "main")))
       (f work remote)
       (finally (fs/delete-tree root))))))

(defn- remote-tags [remote]
  (str/trim (:out (proc/sh ["git" "-C" remote "tag" "--list"]))))

(deftest bump-release-pr-opens-the-pr-after-the-tag-is-pushed-test
  (with-release-repo
    (fn [work remote]
      (let [calls (atom [])
            exit (atom nil)
            out (with-out-str
                  (bump/bump-cmd {:opts {:root work :apply true :release-pr true}
                                  :exit! #(reset! exit %)
                                  :release-pr! (fn [dir opts]
                                                 (swap! calls conj {:dir dir :opts opts
                                                                    :remote-tags (remote-tags remote)})
                                                 (r/ok {:status :created :url pr-url}))}))]
        (is (nil? @exit) "the bump succeeds")
        (is (= [{:dir (str (fs/canonicalize work)) :opts {:tag "v0.4.3"} :remote-tags "v0.4.3"}]
               @calls)
            "the forge is asked once, for the pushed tag, after the remote holds it")
        (is (str/includes? out "Done: v0.4.3"))))))

(deftest bump-without-release-pr-leaves-the-forge-alone-test
  (with-release-repo
    (fn [work remote]
      (let [calls (atom [])]
        (with-out-str
          (bump/bump-cmd {:opts {:root work :apply true}
                          :exit! (fn [_])
                          :release-pr! (forge-stub calls)}))
        (is (= "v0.4.3" (remote-tags remote)) "the release itself still ships")
        (is (= [] @calls))))))

(deftest bump-release-pr-without-a-wired-forge-warns-test
  (with-release-repo
    (fn [work _]
      (let [exit (atom nil)
            out (with-out-str
                  (bump/bump-cmd {:opts {:root work :apply true :release-pr true}
                                  :exit! #(reset! exit %)}))]
        (is (nil? @exit) "a missing opener never fails the bump")
        (is (str/includes? out "open the PR by hand"))))))

(deftest bump-release-pr-skips-a-rejected-push-test
  (with-release-repo true
    (fn [work remote]
      (let [calls (atom [])
            exit (atom nil)]
        (with-out-str
          (bump/bump-cmd {:opts {:root work :apply true :release-pr true}
                          :exit! #(reset! exit %)
                          :release-pr! (forge-stub calls)}))
        (is (= 1 @exit))
        (is (= "" (remote-tags remote)))
        (is (= [] @calls) "no pull request for a release that never reached the remote")))))
