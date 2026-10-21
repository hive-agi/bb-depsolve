(ns bb-depsolve.version.forge-map-trifecta-test
  "Bare-group coordinates on a self-hosted gitea resolve through a forge-map.

   `general/srelaying` is not io.<forge>.<org>/<repo>; the workspace keeps the
   short group on purpose. A forge-map entry for the prefix names the forge,
   its host and the org, and the clone URL is the gitea SSH one. The
   io.github / io.gitlab / io.codeberg coordinates resolve exactly as before."
  (:require [bb-depsolve.version.api :as v]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

(def ^:private forge-map
  {"general/" {:forge :gitea :host "gitea.hive-mcp.com" :org "buddhilw"}})

(defn- clone-url-of
  "Trifecta subject: lib + forge-map -> clone URL, or nil when unresolvable."
  [{:keys [lib forge-map]}]
  (when-let [{:keys [forge org repo host]} (v/parse-forge-lib lib forge-map)]
    (v/forge-clone-url forge org repo host)))

(tri/deftrifecta forge-map-clone-url-trifecta
  clone-url-of
  {:golden-path "test/golden/bb-depsolve/forge-map-clone-url.edn"
   :cases {:gitea-bare-group  {:lib 'general/srelaying :forge-map forge-map}
           :gitea-no-map      {:lib 'general/srelaying :forge-map nil}
           :github            {:lib 'io.github.hive-agi/hive-mcp :forge-map forge-map}
           :github-no-map     {:lib 'io.github.hive-agi/hive-mcp :forge-map nil}
           :gitlab            {:lib 'io.gitlab.myorg/myrepo :forge-map forge-map}
           :codeberg          {:lib 'io.codeberg.myorg/tool :forge-map nil}
           :unmapped-group    {:lib 'cheshire/cheshire :forge-map forge-map}
           :second-gitea      {:lib 'labs/probe
                               :forge-map (assoc forge-map "labs/"
                                                 {:forge :gitea :host "git.example.org" :org "lab"})}}
   :gen (gen/let [group (gen/elements ["general" "other" "io.github.acme"])
                  repo  (gen/not-empty gen/string-alphanumeric)]
          {:lib (symbol group (str "r" repo)) :forge-map forge-map})
   :pred #(or (nil? %) (string? %))
   :num-tests 100
   :mutations [["ignores-forge-map" (fn [{:keys [lib]}]
                                      (when-let [{:keys [forge org repo]} (v/parse-forge-lib lib)]
                                        (v/forge-clone-url forge org repo)))]
               ["https-not-ssh" (fn [{:keys [lib forge-map]}]
                                  (when-let [{:keys [org repo host]} (v/parse-forge-lib lib forge-map)]
                                    (format "https://%s/%s/%s" (or host "github.com") org repo)))]]})

(deftest general-srelaying-resolves-to-its-gitea-ssh-url
  (is (= {:forge :gitea :host "gitea.hive-mcp.com" :org "buddhilw" :repo "srelaying"}
         (v/parse-forge-lib 'general/srelaying forge-map)))
  (is (= "git@gitea.hive-mcp.com:buddhilw/srelaying.git"
         (v/forge-clone-url :gitea "buddhilw" "srelaying" "gitea.hive-mcp.com"))))

(deftest forge-libs-are-unchanged
  (testing "the one-arity and a forge-map both leave io.<forge> coordinates alone"
    (is (= {:forge :github :org "hive-agi" :repo "hive-mcp"}
           (v/parse-forge-lib 'io.github.hive-agi/hive-mcp)
           (v/parse-forge-lib 'io.github.hive-agi/hive-mcp forge-map)))
    (is (= "https://github.com/hive-agi/hive-mcp"
           (v/forge-clone-url :github "hive-agi" "hive-mcp")
           (v/forge-clone-url :github "hive-agi" "hive-mcp" nil)))))

(deftest a-gitea-coordinate-needs-a-host
  (is (nil? (v/forge-clone-url :gitea "buddhilw" "srelaying")))
  (is (nil? (v/forge-clone-url :gitea "buddhilw" "srelaying" nil)))
  (is (nil? (v/parse-forge-lib 'general/srelaying {"general/" {:forge :gitea :org "buddhilw"}}))
      "an entry without a host cannot build a URL, so it does not resolve"))
