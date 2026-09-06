(ns bb-depsolve.version.repos
  "Which Maven repositories a consumer reaches, and how a resolution is seen
   through them. Pure.

   Contract: a resolver reads the UNION of every registry it holds credentials
   for; a consumer can only fetch from the registries its own dep file declares
   under `:mvn/repos` (plus the public defaults). A version pin is legal only
   when the version is reachable from the CONSUMER's registries, so every
   resolution is projected per consumer before it becomes a pin."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [bb-depsolve.schema.api]
            [bb-depsolve.version.semver :as semver]))

(def public-repo-hosts
  "Hosts whose artifacts need no private credentials."
  #{"repo1.maven.org" "repo.maven.apache.org" "central.sonatype.com"
    "oss.sonatype.org" "s01.oss.sonatype.org" "repo.clojars.org" "clojars.org"})

(defn public-repo?
  "True when URL is a well-known public Maven host. Total: true for nil/blank,
   so an unusable entry is never mistaken for a private registry."
  [url]
  (if (str/blank? url)
    true
    (boolean (some #(str/includes? url %) public-repo-hosts))))

(defn repos-in
  "Every `:mvn/repos` entry declared by one parsed dep-file map, top level and
   under any alias. Returns [{:id string :url string} ...]. Total: [] on nil."
  [dep-edn]
  (when (map? dep-edn)
    (into []
          (comp (mapcat #(seq (:mvn/repos %)))
                (keep (fn [[id {:keys [url]}]]
                        (when (and id url)
                          {:id (name id) :url url}))))
          (cons dep-edn (vals (:aliases dep-edn))))))

(defn private-repos
  "Distinct non-public repositories declared across DEP-EDNS, in declaration
   order. Returns [{:id :url} ...]."
  [dep-edns]
  (into []
        (comp (mapcat repos-in)
              (remove #(public-repo? (:url %)))
              (distinct))
        dep-edns))

(defn read-dep-content
  "The first form of a dep file's CONTENT as data, or nil when unreadable.
   Uses the Clojure reader with eval disabled rather than the EDN reader: a
   bb.edn task may carry `#(...)` or `@x`, which EDN refuses but the file
   still declares repositories."
  [content]
  (try
    (binding [*read-eval* false]
      (read-string content))
    (catch Exception _ nil)))

(defn declared-repos
  "The `:mvn/repos` entries a dep file's CONTENT declares, top level and under
   aliases. Total: [] when the content is unreadable."
  [content]
  (or (repos-in (read-dep-content content)) []))

(defn- norm-url
  [u]
  (str/replace (str u) #"/+$" ""))

(defn reachable?
  "True when a consumer declaring CONSUMER-REPOS can fetch from the registry a
   registry-version names. A public registry is always reachable; a private
   one only when the consumer declares it, by url or by repository id."
  [consumer-repos {:keys [id url public?]}]
  (boolean
   (or public?
       (some (fn [{cid :id curl :url}]
               (or (and url curl (= (norm-url url) (norm-url curl)))
                   (and id cid (= id cid))))
             consumer-repos))))

(defn newest
  "The registry-version carrying the highest version, or nil when empty."
  [registry-versions]
  (last (sort-by :version semver/version-compare registry-versions)))

(defn reachable-versions
  "The registry-versions of BY-REGISTRY a consumer declaring CONSUMER-REPOS can
   fetch."
  [consumer-repos by-registry]
  (filterv #(reachable? consumer-repos %) by-registry))

(defn project-lib
  "RESOLVED-LIB as one consumer declaring CONSUMER-REPOS sees it.

   Without registry detail (:mvn-by-registry / :mvn-unread) the lib is
   returned unchanged. Otherwise :mvn-version becomes the newest REACHABLE
   version, :mvn-source names the registry it comes from, and
   :mvn-unreachable lists newer versions the consumer cannot fetch.

   A reachable registry that did not answer makes the projection UNCERTAIN:
   no :mvn-version is chosen (a stale or partial read must never move a pin)
   and :mvn-uncertain names the registries. A lib with nothing to pin and no
   git coordinate projects to nil."
  [resolved-lib consumer-repos]
  (if (or (:mvn-by-registry resolved-lib) (:mvn-unread resolved-lib))
    (let [by-registry (or (:mvn-by-registry resolved-lib) [])
          uncertain (filterv #(reachable? consumer-repos %) (:mvn-unread resolved-lib))
          chosen (when (empty? uncertain)
                   (newest (reachable-versions consumer-repos by-registry)))
          unreachable (->> by-registry
                           (remove #(reachable? consumer-repos %))
                           (filter #(or (nil? chosen)
                                        (pos? (semver/version-compare (:version %) (:version chosen)))))
                           (sort-by :version semver/version-compare)
                           vec)
          projected (cond-> (dissoc resolved-lib :mvn-version :mvn-source :mvn-unreachable :mvn-uncertain)
                      chosen (assoc :mvn-version (:version chosen) :mvn-source (:id chosen))
                      (seq unreachable) (assoc :mvn-unreachable unreachable)
                      (seq uncertain) (assoc :mvn-uncertain uncertain))]
      (when (or chosen (and (:tag projected) (:sha projected)))
        projected))
    resolved-lib))

(defn project-resolved
  "RESOLVED as one consumer sees it. Libs the consumer can pin nothing for drop
   out of the map."
  [resolved consumer-repos]
  (into {}
        (keep (fn [[lib resolved-lib]]
                (when-let [projected (project-lib resolved-lib consumer-repos)]
                  [lib projected])))
        resolved))

(defn withheld
  "Libs among PINNED-LIBS that a consumer declaring CONSUMER-REPOS pins by
   Maven coordinate but that sync will not move, each with its reason:
     {:lib :reason :unread      :unread [registry-read-failure]}
       a registry the consumer reaches did not answer, so nothing is certain
     {:lib :reason :unreachable :versions [registry-version]}
       every published version sits on a registry the consumer does not declare
   Sorted by lib."
  [resolved consumer-repos pinned-libs]
  (->> pinned-libs
       (keep (fn [lib]
               (let [{:keys [mvn-by-registry mvn-unread]} (get resolved lib)
                     uncertain (filterv #(reachable? consumer-repos %) mvn-unread)]
                 (cond
                   (seq uncertain)
                   {:lib lib :reason :unread :unread uncertain}

                   (and (seq mvn-by-registry)
                        (empty? (reachable-versions consumer-repos mvn-by-registry)))
                   {:lib lib :reason :unreachable :versions (vec mvn-by-registry)}))))
       (sort-by (comp str :lib))
       vec))

(m/=> public-repo? [:=> [:cat [:maybe :string]] :boolean])

(m/=> repos-in
      [:=> [:cat [:maybe :any]] [:maybe [:vector :bb-depsolve/mvn-repo]]])

(m/=> private-repos
      [:=> [:cat [:sequential :any]] [:vector :bb-depsolve/mvn-repo]])

(m/=> declared-repos
      [:=> [:cat [:maybe :string]] [:vector :bb-depsolve/mvn-repo]])

(m/=> reachable?
      [:=> [:cat [:sequential :bb-depsolve/mvn-repo] :bb-depsolve/registry-version] :boolean])

(m/=> project-lib
      [:=> [:cat :bb-depsolve/resolved-lib [:sequential :bb-depsolve/mvn-repo]]
       [:maybe :bb-depsolve/resolved-lib]])

(m/=> project-resolved
      [:=> [:cat :bb-depsolve/resolved [:sequential :bb-depsolve/mvn-repo]]
       :bb-depsolve/resolved])
