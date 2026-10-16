(ns bb-depsolve.core.upgrade-unread-trifecta-test
  "Golden, property and mutation checks for a consumer's unread registry hold."
  (:require [bb-depsolve.core.upgrade.guard :as guard]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

(def ^:private public {:id "clojars" :url "https://repo.clojars.org" :public? true})
(def ^:private private {:id "gitea" :url "https://git.example.com/maven" :public? false})
(def ^:private declared [{:id "gitea" :url "https://git.example.com/maven"}])
(def ^:private failure (assoc private :error :io/unread :status 503))

(defn- plan-from
  "Build the consumer plan from the registry's observed failure and the dep file's declarations."
  [declares-private? unread?]
  (guard/plan [{:path "/w/deps.edn" :project "w" :lib 'acme/lib
                :version "1.0.0" :consumer-repos (if declares-private? declared [])}]
              {'acme/lib {:by-registry [(assoc public :version "1.1.0")]
                          :unread (if unread? [failure] [])}} {}))

(defn- plan-one
  "Project the same resolution for one pinned lib and one consumer."
  [{:keys [declares-private? unread?]}]
  (plan-from declares-private? unread?))

(defn- safe-plan?
  "An unread reachable registry never permits an upgrade; otherwise the
   answered public version may advance."
  [{:keys [declares-private? unread?] :as input}]
  (let [{:keys [upgrades held]} (plan-one input)]
    (if (and declares-private? unread?)
      (and (empty? upgrades) (= :unread (:reason (first held)))
           (= [failure] (:unread (first held))))
      (and (= 1 (count upgrades)) (empty? held)))))

(defn- ignore-unread
  "Broken: treats the private registry's failed read as absence."
  [{:keys [declares-private?]}]
  (plan-from declares-private? false))

(defn- hold-every-unread
  "Broken: holds public-only consumers for an undeclared private registry."
  [{:keys [unread?]}]
  (plan-from true unread?))

(tri/deftrifecta upgrade-unread-trifecta
  plan-one
  {:golden-path "test/golden/bb-depsolve/upgrade-unread.edn"
   :cases {:reachable-unread {:declares-private? true :unread? true}
           :unreachable-unread {:declares-private? false :unread? true}
           :all-answered {:declares-private? true :unread? false}}
   :gen (gen/let [declares-private? gen/boolean unread? gen/boolean]
          {:declares-private? declares-private? :unread? unread?})
   :pred safe-plan?
   :num-tests 60
   :mutations [["ignore-unread" ignore-unread]
               ["hold-every-unread" hold-every-unread]]})