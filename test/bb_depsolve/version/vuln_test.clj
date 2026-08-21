(ns bb-depsolve.version.vuln-test
  "Tests for OSV vulnerability parsing and the fix-version recommendation.

   The recommendation is what `audit --fix` writes into a dep file, so a wrong
   answer here silently ships a still-vulnerable version."
  (:require [clojure.test :refer [deftest is testing]]
            [bb-depsolve.version.vuln :as vuln]))

(defn- vuln-entry
  "An OSV record shaped like the API returns one."
  [{:keys [id aliases severity cvss fixed]}]
  (cond-> {:id id
           :aliases (or aliases [])
           :database_specific {:severity severity}
           :affected [{:ranges [{:type "ECOSYSTEM"
                                 :events (into [{:introduced "0"}]
                                               (map (fn [f] {:fixed f}) fixed))}]}]}
    cvss (assoc :severity [{:type "CVSS_V3" :score cvss}])))

(deftest parses-cvss-vector-alongside-the-qualitative-label
  (testing "the label comes from database_specific, the vector from severity[]"
    (let [parsed (vuln/parse-osv-vuln
                  (vuln-entry {:id "GHSA-xxxx" :aliases ["CVE-2026-1"]
                               :severity "HIGH"
                               :cvss "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:H"
                               :fixed ["1.2.3"]}))]
      (is (= "CVE-2026-1" (:id parsed)) "a CVE alias is preferred as the id")
      (is (= "GHSA-xxxx" (:ghsa parsed)))
      (is (= "HIGH" (:severity parsed)))
      (is (= "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:H" (:cvss parsed)))
      (is (= ["1.2.3"] (:fixed-in parsed))))))

(deftest cvss-is-absent-when-osv-publishes-none
  (is (nil? (:cvss (vuln/parse-osv-vuln (vuln-entry {:id "GHSA-y" :severity "LOW" :fixed ["1.0.1"]}))))))

(deftest recommends-the-lowest-version-clearing-one-vuln
  (is (= "42.7.11"
         (vuln/recommended-fix "42.7.7" [{:fixed-in ["42.7.11" "43.0.0"]}]))))

(deftest recommends-the-version-that-clears-every-vuln
  (testing "a version fixing only the first advisory still ships the second"
    (is (= "42.7.12"
           (vuln/recommended-fix "42.7.7" [{:fixed-in ["42.7.11"]}
                                           {:fixed-in ["42.7.12"]}])))))

(deftest ignores-fixed-versions-not-newer-than-the-current-pin
  (testing "a fix released before the pinned version is not a downgrade target"
    (is (= "2.0.0"
           (vuln/recommended-fix "1.5.0" [{:fixed-in ["1.0.1" "2.0.0"]}])))))

(deftest nil-when-a-vuln-publishes-no-usable-fix
  (testing "unfixed advisory -> no version bump can clear it"
    (is (nil? (vuln/recommended-fix "1.0.0" [{:fixed-in ["1.0.1"]} {:fixed-in []}])))
    (is (nil? (vuln/recommended-fix "9.9.9" [{:fixed-in ["1.0.1"]}])))))

(deftest total-on-degenerate-input
  (is (nil? (vuln/recommended-fix nil [{:fixed-in ["1.0.0"]}])))
  (is (nil? (vuln/recommended-fix "1.0.0" [])))
  (is (nil? (vuln/parse-osv-vuln nil)))
  (is (nil? (vuln/parse-osv-vuln "not-a-map"))))
