(ns bb-depsolve.audit-test
  "Tests for the OSV.dev vulnerability query boundary."
  (:require [babashka.http-client :as http]
            [bb-depsolve.audit :as audit]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is]]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private osv-body
  (json/generate-string
   {:vulns [{:id "GHSA-xxxx-yyyy-zzzz"
             :summary "Remote code execution"
             :severity [{:type "CVSS_V3" :score "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H"}]}]}))

(def ^:private query-osv #'audit/query-osv)

;; =============================================================================
;; Unit — query-osv
;; =============================================================================

(deftest query-osv-reports-the-vulnerabilities-osv-returns-test
  (with-redefs [http/post (fn [& _] {:status 200 :body osv-body})]
    (let [{:keys [ok]} (query-osv "org.clojure" "clojure" "1.12.1")]
      (is (= 1 (count ok))
          "a 200 answer must not be read as an empty vulnerability list")
      (is (= "GHSA-xxxx-yyyy-zzzz" (:id (first ok)))))))

(deftest query-osv-treats-a-non-200-as-no-findings-test
  (with-redefs [http/post (fn [& _] {:status 503 :body ""})]
    (is (= {:ok []} (query-osv "org.clojure" "clojure" "1.12.1")))))

(deftest query-osv-errs-when-the-request-throws-test
  (with-redefs [http/post (fn [& _] (throw (ex-info "connection reset" {})))]
    (is (= :io/osv-query (:error (query-osv "org.clojure" "clojure" "1.12.1"))))))
