(ns bb-depsolve.release.pr-body-test
  (:require [bb-depsolve.release.pr-body :as body]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private checkpoint
  {:status :aborted
   :released {"mid" "1.2.0"}
   :waves [{:index 0
            :steps [{:project "mid" :status :released :version "1.2.0"
                     :pin-updates [{:dep "weave" :lib 'io.github.hive-agi/hive-weave
                                    :coord :mvn :path "mid/deps.edn"
                                    :from "0.4.1" :to "0.4.2"}
                                   {:dep "weave" :lib 'io.github.hive-agi/hive-weave
                                    :coord :mvn :path "mid/bb.edn"
                                    :from "0.4.1" :to "0.4.2"}
                                   {:dep "noop" :from "1.0.0" :to "1.0.0"}
                                   {:dep "pending" :from "1.0.0" :to nil}]}]}
           {:index 1
            :steps [{:project "top" :status :release-failed
                     :pin-updates [{:dep "mid" :lib 'io.github.hive-agi/mid
                                    :coord :git :path "top/deps.edn"
                                    :from "v1.1.0" :to "v1.2.0"}]}
                    {:project "broken" :status :sync-failed
                     :pin-updates [{:dep "mid" :from "v1.1.0" :to "v1.2.0"}]}]}]})

(def ^:private expected-groups
  [{:type :feat :subjects ["feat(api)!: expose release status"
                          "feat(cli): show release notes"]}
   {:type :fix :subjects ["fix(sync): retain pin aliases"
                         "fix: preserve tags on retry"]}
   {:type :refactor :subjects ["refactor(plan): simplify wave collection"]}
   {:type :test :subjects ["test: cover checkpoint resume"]}
   {:type :chore :subjects ["chore(deps): bump internal dependencies"]}
   {:type :other :subjects ["release: v1.2.0"
                           "Merge pull request #12 from hive-agi/release-notes"]}])

(deftest fixture-git-logs-test
  (doseq [fixture ["subjects.log" "oneline.log"]]
    (testing fixture
      (is (= expected-groups
             (body/group-commits (slurp (str "test/fixtures/release-pr/" fixture)))))))
  (testing "deduplication precedes the two-subject limit and stays deterministic"
    (is (= expected-groups
           (body/group-commits (slurp "test/fixtures/release-pr/subjects.log")))))
  (testing "empty inputs do not invent commits"
    (is (= [] (body/group-commits nil)))
    (is (= [] (body/group-commits "\n  \n")))))

(deftest conventional-prefixes-test
  (is (= [{:type :feat :subjects ["FEAT(core): uppercase"]}
          {:type :fix :subjects ["fix!: breaking fix"]}
          {:type :other :subjects ["feature: not feat" "fixup: not fix"]}]
         (body/group-commits
          (str "FEAT(core): uppercase\nfix!: breaking fix\n"
               "feature: not feat\nfixup: not fix\nfix(no colon)\n")))))

(deftest checkpoint-ripple-test
  (is (= [{:project "mid" :dep "weave" :lib 'io.github.hive-agi/hive-weave
           :from "0.4.1" :to "0.4.2"}
          {:project "top" :dep "mid" :lib 'io.github.hive-agi/mid
           :from "v1.1.0" :to "v1.2.0"}]
         (body/ripple-updates checkpoint)))
  (is (= (body/ripple-updates checkpoint)
         (body/ripple-updates (update checkpoint :waves reverse))))
  (is (= [] (body/ripple-updates nil)))
  (is (= [] (body/ripple-updates {:waves [] :released {"only-a-seed" "1.0.0"}}))))

(deftest rendered-body-test
  (let [rendered (body/render {:tag "v1.2.0" :version "1.2.0" :previous-version "1.1.0"
                               :base "main" :head "minor-release"
                               :git-log (slurp "test/fixtures/release-pr/subjects.log")
                               :checkpoint checkpoint
                               :pr-template {:test-plan "- [ ] Run the release smoke test."}})]
    (is (str/includes? rendered "VERSION: `1.1.0` -> `1.2.0`."))
    (is (str/includes? rendered "Release v1.2.0 landed on `minor-release`"))
    (is (str/includes? rendered "### Features\n\n- feat(api)!: expose release status"))
    (is (not (str/includes? rendered "older feature omitted")))
    (is (str/includes? rendered "## Sync ripple\n\n- `mid`: `io.github.hive-agi/hive-weave` `0.4.1` -> `0.4.2`."))
    (is (not (str/includes? rendered "broken")))
    (is (str/includes? rendered "## Test plan\n\n- [ ] Run the release smoke test."))
    (is (= 1 (count (re-seq #"feat\(api\)!: expose release status" rendered))))))

(deftest missing-facts-and-test-plan-default-test
  (doseq [test-plan [nil "" "  \n" false 42]]
    (let [rendered (body/render {:base "main" :head "release"
                                 :pr-template {:test-plan test-plan}})]
      (is (str/includes? rendered body/default-test-plan))
      (is (str/includes? rendered "No commit subjects recorded"))
      (is (str/includes? rendered "No internal dependency pin updates recorded"))
      (is (not (str/includes? rendered "VERSION:"))))))
