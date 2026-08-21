(ns bb-depsolve.core.clojars-resolve-test
  "Clojars resolution reads the repository's own maven-metadata.xml.
   The JSON API's `latest_release` names the newest UPLOAD, pre-releases
   included, and is a fallback that must still honour allow-pre?."
  (:require [babashka.http-client :as http]
            [bb-depsolve.core.resolve.registries :as registries]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(def ^:private metadata-xml
  (str "<metadata><groupId>metosin</groupId><artifactId>reitit</artifactId>"
       "<versioning><latest>0.11.0-rc1</latest><release>0.11.0-rc1</release>"
       "<versions><version>0.7.2</version><version>0.9.1</version>"
       "<version>0.11.0-rc1</version></versions></versioning></metadata>"))

(def ^:private api-json
  "{\"latest_release\":\"0.11.0-rc1\",\"recent_versions\":[{\"version\":\"0.11.0-rc1\"},{\"version\":\"0.9.1\"}]}")

(defn- transport
  "Stub http/get: :metadata answers repo.clojars.org, :api answers the JSON API.
   A source left out answers 404."
  [{:keys [metadata api]}]
  (fn [url & _]
    (cond
      (str/includes? url "repo.clojars.org") (or metadata {:status 404 :body ""})
      (str/includes? url "clojars.org/api") (or api {:status 404 :body ""})
      :else {:status 404 :body ""})))

;; =============================================================================
;; resolve-clojars-latest
;; =============================================================================

(deftest the-repository-listing-outranks-a-pre-release-latest-release-test
  (with-redefs [http/get (transport {:metadata {:status 200 :body metadata-xml}
                                     :api {:status 200 :body api-json}})]
    (is (= {:ok "0.9.1"} (registries/resolve-clojars-latest "metosin" "reitit"))
        "the newest STABLE version in <versions>, not the newest upload")))

(deftest allow-pre-takes-the-release-candidate-test
  (with-redefs [http/get (transport {:metadata {:status 200 :body metadata-xml}})]
    (is (= {:ok "0.11.0-rc1"} (registries/resolve-clojars-latest "metosin" "reitit" true)))))

(deftest the-api-fallback-refuses-a-pre-release-test
  (testing "metadata unreachable, API reports a release candidate"
    (with-redefs [http/get (transport {:api {:status 200 :body api-json}})]
      (let [{:keys [error]} (registries/resolve-clojars-latest "metosin" "reitit")]
        (is (some? error)
            "a pre-release latest_release is not a published version")))))

(deftest the-api-fallback-serves-allow-pre-test
  (with-redefs [http/get (transport {:api {:status 200 :body api-json}})]
    (is (= {:ok "0.11.0-rc1"} (registries/resolve-clojars-latest "metosin" "reitit" true)))))

(deftest an-artifact-clojars-does-not-carry-errs-test
  (with-redefs [http/get (transport {})]
    (is (r/err? (registries/resolve-clojars-latest "io.milvus" "milvus-sdk-java")))))

;; =============================================================================
;; resolve-clojars-versions
;; =============================================================================

(deftest versions-come-from-the-complete-repository-listing-test
  (with-redefs [http/get (transport {:metadata {:status 200 :body metadata-xml}
                                     :api {:status 200 :body api-json}})]
    (is (= {:ok #{"0.7.2" "0.9.1" "0.11.0-rc1"}}
           (registries/resolve-clojars-versions "metosin" "reitit"))
        "the API reports at most five recent uploads; the listing is complete")))

(deftest versions-fall-back-to-the-api-test
  (with-redefs [http/get (transport {:api {:status 200 :body api-json}})]
    (is (= {:ok #{"0.11.0-rc1" "0.9.1"}}
           (registries/resolve-clojars-versions "metosin" "reitit")))))

;; =============================================================================
;; resolve-mvn-latest — the fleet-visible consequence
;; =============================================================================

(deftest a-library-whose-newest-upload-is-a-candidate-stays-visible-test
  (with-redefs [http/get (transport {:metadata {:status 200 :body metadata-xml}
                                     :api {:status 200 :body api-json}})]
    (is (= {:ok "0.9.1"} (registries/resolve-mvn-latest 'metosin/reitit false))
        "reading latest_release lost the library entirely once an rc was cut")))
