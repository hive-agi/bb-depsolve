(ns bb-depsolve.core.registry-test
  "Private-registry discovery: a repository is only a resolution source when a
   dep file declares its url AND settings.xml holds credentials under the same
   repository id. Schemas are imported from bb-depsolve.schema.api — never
   redefined here."
  (:require [bb-depsolve.core.registry :as registry]
            [bb-depsolve.schema.api]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]))

(def ^:private gitea-url
  "https://forge.example/api/packages/acme/maven")

(def ^:private settings-xml
  (str "<settings><servers>"
       "<server><id>acme-forge</id><username>u</username><password>p</password></server>"
       "<server><id>no-creds</id><username></username><password></password></server>"
       "</servers></settings>"))

(deftest public-repos-are-never-private-registries
  (testing "the well-known hosts need no credentials"
    (is (registry/public-repo? "https://repo1.maven.org/maven2/"))
    (is (registry/public-repo? "https://repo.clojars.org/")))
  (testing "an unusable entry is public, so it is never mistaken for private"
    (is (registry/public-repo? nil))
    (is (registry/public-repo? "")))
  (testing "anything else is a candidate"
    (is (not (registry/public-repo? gitea-url)))))

(deftest repos-are-read-from-the-top-level-and-from-aliases
  (let [dep-edn {:mvn/repos {"acme-forge" {:url gitea-url}
                             "central" {:url "https://repo1.maven.org/maven2/"}}
                 :aliases {:build {:mvn/repos {"alias-forge" {:url "https://other.example/m2"}}}}}]
    (is (= #{"acme-forge" "central" "alias-forge"}
           (set (map :id (registry/repos-in dep-edn))))
        "a repo declared only under an alias is still declared")
    (is (= [{:id "acme-forge" :url gitea-url}
            {:id "alias-forge" :url "https://other.example/m2"}]
           (registry/private-repos [dep-edn]))
        "central drops out; declaration order is kept")
    (testing "totality"
      (is (nil? (registry/repos-in nil)))
      (is (= [] (registry/private-repos []))))))

(deftest private-repos-are-deduplicated-across-dep-files
  (let [a {:mvn/repos {"acme-forge" {:url gitea-url}}}
        b {:mvn/repos {"acme-forge" {:url gitea-url}}}]
    (is (= 1 (count (registry/private-repos [a b])))
        "the fleet declares the same registry in every consumer's deps.edn")))

(deftest settings-servers-parse-into-credentials
  (let [servers (registry/parse-settings-servers settings-xml)]
    (is (= {:username "u" :password "p"} (get servers "acme-forge")))
    (is (nil? (get servers "no-creds"))
        "a server with blank credentials carries none")
    (testing "totality"
      (is (= {} (registry/parse-settings-servers nil)))
      (is (= {} (registry/parse-settings-servers "<not-xml"))))))

(deftest a-registry-needs-both-halves
  (let [repos [{:id "acme-forge" :url gitea-url}
               {:id "unknown-forge" :url "https://other.example/m2"}]
        servers (registry/parse-settings-servers settings-xml)
        joined (registry/with-credentials repos servers)]
    (is (= [{:id "acme-forge" :url gitea-url :username "u" :password "p"}] joined)
        "a repo with no matching <server> is not a resolution source")
    (testing "the join satisfies the schema the src declares"
      (is (every? #(m/validate :bb-depsolve/private-registry %) joined)))
    (testing "no credentials at all yields no registry, rather than a url-only one"
      (is (= [] (registry/with-credentials repos {}))))))

(deftest discovery-reads-a-real-workspace
  (let [root (str (java.nio.file.Files/createTempDirectory
                   "depsolve-registry" (into-array java.nio.file.attribute.FileAttribute [])))
        settings (str root "/settings.xml")]
    (spit (str root "/deps.edn")
          (pr-str {:mvn/repos {"acme-forge" {:url gitea-url}}
                   :deps {}}))
    (spit settings settings-xml)
    (testing "the url comes from the workspace, the credentials from settings.xml"
      (is (= {:id "acme-forge" :url gitea-url :username "u" :password "p"}
             (registry/discover {:root root :depth 0 :settings settings}))))
    (testing "no settings.xml means no private registry, not a crash"
      (is (nil? (registry/discover {:root root :depth 0
                                    :settings (str root "/absent.xml")}))))))
