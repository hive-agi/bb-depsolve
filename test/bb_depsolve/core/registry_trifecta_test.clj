(ns bb-depsolve.core.registry-trifecta-test
  "Property + mutation coverage for private-registry discovery.

   The schemas come from bb-depsolve.schema.api — this namespace declares none
   of its own, so a schema that drifts from the join fails here rather than in
   a release. The mutation facets pin the two regressions that would make the
   discovery useless: emitting a registry we hold no credentials for (the very
   bug that left the fleet's proprietary libs unresolvable), and treating a
   public host as private."
  (:require [bb-depsolve.core.registry :as registry]
            [bb-depsolve.schema.api :as sch]
            [clojure.string :as str]
            [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-schemas.schema :as hs]
            [hive-schemas.test :as hst]
            [hive-test.mutation :as mut]
            [hive-test.trifecta :as tri]))

(sch/register!)

;; =============================================================================
;; Predicates under test — each is the contract, stated once
;; =============================================================================

(defn private-registry?
  "A resolution source names its repository, its url, and BOTH halves of the
   credentials. A url-only registry is not one."
  [x]
  (hs/validate :bb-depsolve/private-registry x))

(defn mvn-repo?
  "A declared repository carries an id and a url."
  [x]
  (hs/validate :bb-depsolve/mvn-repo x))

;; =============================================================================
;; Schema-synthesized predicate trifectas — generator, oracle and corruptions
;; all derived from the registered schema
;; =============================================================================

(hst/deftrifecta-predicate private-registry-contract
  bb-depsolve.core.registry-trifecta-test/private-registry?
  {:schema :bb-depsolve/private-registry})

(hst/deftrifecta-predicate mvn-repo-contract
  bb-depsolve.core.registry-trifecta-test/mvn-repo?
  {:schema :bb-depsolve/mvn-repo})

;; =============================================================================
;; with-credentials — the join, generated from its own schemas
;;
;; :mutation is off: [:vector private-registry] yields no schema-derived
;; corruption of the OUTPUT (every field is required, so a mutant vector is
;; simply not a vector of registries and the synthesizer says so rather than
;; passing vacuously). The mutation coverage for this fn is explicit, below.
;; =============================================================================

(defn- join-is-sound?
  "The join keeps exactly the declared repositories whose id settings.xml
   knows, in declaration order, each carrying ITS OWN credentials. Stated
   positionally so repeated ids — which a dep file may legitimately produce —
   are related rather than collapsed."
  [[repos servers] out]
  (let [kept (filterv #(contains? servers (:id %)) repos)]
    (and (= (count kept) (count out))
         (every? true?
                 (map (fn [repo registry]
                        (and (= (:id repo) (:id registry))
                             (= (:url repo) (:url registry))
                             (= (get servers (:id repo))
                                (select-keys registry [:username :password]))))
                      kept out)))))

(hst/deftrifecta-from-schema with-credentials-trifecta
  bb-depsolve.core.registry/with-credentials
  {:in  [:cat
         [:vector :bb-depsolve/mvn-repo]
         [:map-of :bb-depsolve/repo-id [:map [:username :string] [:password :string]]]]
   :out [:vector :bb-depsolve/private-registry]
   :rel join-is-sound?
   :mutation false
   :num-tests 100})

;; =============================================================================
;; public-repo? — the gate that decides what is even a candidate
;; =============================================================================

(def ^:private gen-url
  "Urls spanning every branch: the public hosts, private forges, and the
   unusable values a malformed dep file yields."
  (gen/one-of
   [(gen/elements (vec registry/public-repo-hosts))
    (gen/fmap #(str "https://" % "/maven2/") (gen/elements (vec registry/public-repo-hosts)))
    (gen/elements ["https://forge.example/api/packages/acme/maven"
                   "https://nexus.internal:8081/repository/releases"
                   "file:///srv/m2"])
    (gen/return nil)
    (gen/return "")
    gen/string-alphanumeric]))

(tri/deftrifecta public-repo-trifecta
  bb-depsolve.core.registry/public-repo?
  {:golden-path "test/golden/bb-depsolve/registry-public-repo.edn"
   :cases     {:central  "https://repo1.maven.org/maven2/"
               :clojars  "https://repo.clojars.org/"
               :sonatype "https://oss.sonatype.org/content/repositories/snapshots/"
               :private  "https://forge.example/api/packages/acme/maven"
               :nil      nil
               :blank    ""}
   :gen       gen-url
   :pred      boolean?
   :num-tests 300
   :mutations [["treats-every-url-as-private"
                (fn [_] false)]
               ["treats-every-url-as-public"
                (fn [_] true)]
               ["blank-url-counted-as-private"
                ;; drops the totality guard, so a malformed dep file entry
                ;; becomes a registry we then try to authenticate to
                (fn [url]
                  (boolean (some #(and url (str/includes? url %))
                                 registry/public-repo-hosts)))]]})

;; =============================================================================
;; Mutation: the regressions that would reopen the bug this ns exists to close
;; =============================================================================

(mut/deftest-mutations with-credentials-mutations-caught
  bb-depsolve.core.registry/with-credentials

  [["emits-url-only-registries"
    ;; The original defect in reverse: a repo is returned even when no
    ;; <server> matches, so resolution 401s instead of being skipped.
    (fn [repos _servers] (vec repos))]

   ["drops-everything"
    (fn [_repos _servers] [])]

   ["ignores-the-id-match"
    ;; Pairs each repo with an arbitrary credential rather than its own.
    (fn [repos servers]
      (let [creds (first (vals servers))]
        (vec (keep #(when creds (merge % creds)) repos))))]]

  (fn []
    (let [repos [{:id "acme-forge" :url "https://forge.example/m2"}
                 {:id "unknown" :url "https://other.example/m2"}]
          servers {"acme-forge" {:username "u" :password "p"}
                   "elsewhere" {:username "x" :password "y"}}
          out (registry/with-credentials repos servers)]
      (is (= [{:id "acme-forge" :url "https://forge.example/m2"
               :username "u" :password "p"}]
             out)
          "only the repo whose id settings.xml knows becomes a resolution source")
      (is (every? private-registry? out)
          "and it satisfies the schema src declares"))))

(mut/deftest-mutations private-repos-mutations-caught
  bb-depsolve.core.registry/private-repos

  [["keeps-public-repos"
    (fn [dep-edns] (vec (distinct (mapcat registry/repos-in dep-edns))))]

   ["forgets-to-deduplicate"
    (fn [dep-edns]
      (vec (remove #(registry/public-repo? (:url %))
                   (mapcat registry/repos-in dep-edns))))]]

  (fn []
    (let [dep-edn {:mvn/repos {"acme-forge" {:url "https://forge.example/m2"}
                               "central" {:url "https://repo1.maven.org/maven2/"}}}
          out (registry/private-repos [dep-edn dep-edn])]
      (is (= [{:id "acme-forge" :url "https://forge.example/m2"}] out)
          "central drops out, and the registry every consumer declares appears once"))))
