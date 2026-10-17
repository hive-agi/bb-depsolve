(ns bb-depsolve.core.remote-provider-trifecta-test
  "Golden, property, and mutant coverage for remote provider classification."
  (:require [bb-depsolve.core.git :as git]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

(defn- known-provider?
  [provider]
  (contains? #{:github :gitea :gitlab :bitbucket :other} provider))

(tri/deftrifecta remote-provider-trifecta
  bb-depsolve.core.git/remote-provider
  {:golden-path "test/golden/bb-depsolve/remote-provider.edn"
   :cases {:github-https "https://github.com/hive-agi/bb-depsolve.git"
           :github-ssh "git@github.com:hive-agi/bb-depsolve.git"
           :gitea-git "git://gitea.example.org/acme/repo.git"
           :gitlab-ssh "ssh://git@gitlab.com/acme/repo.git"
           :bitbucket "https://bitbucket.org/acme/repo.git"
           :unknown "https://example.org/acme/repo.git"
           :malformed "not a remote"}
   :gen (gen/one-of [gen/string-alphanumeric
                     (gen/fmap #(str "https://" % "/repo.git") gen/string-alphanumeric)])
   :pred known-provider?
   :num-tests 100
   :mutations [["every-remote-is-github" (fn [_] :github)]]})

(defn- provider-with-overrides
  "Supply both URL and configured host overrides as one trifecta subject input."
  [{:keys [url overrides]}]
  (git/remote-provider url overrides))

(tri/deftrifecta remote-provider-override-trifecta
  provider-with-overrides
  {:golden-path "test/golden/bb-depsolve/remote-provider-override.edn"
   :cases {:override {:url "https://forge.example.org/acme/repo.git"
                      :overrides {"forge.example.org" :gitea}}
           :built-in-override {:url "git@github.com:acme/repo.git"
                               :overrides {"github.com" :other}}}
   :gen (gen/return {:url "https://forge.example.org/acme/repo.git"
                     :overrides {"forge.example.org" :gitea}})
   :pred known-provider?
   :num-tests 30
   :mutations [["ignores-overrides" (fn [{:keys [url]}] (git/remote-provider url))]]})
