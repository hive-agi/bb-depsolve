(ns bb-depsolve.version.vuln
  "OSV vulnerability parsing and severity ordering. Pure."
  (:require [clojure.string :as str]))

(defn parse-osv-vuln
  "Parse a single OSV vulnerability entry into a normalized map.
   Total: returns nil for unparseable input."
  [vuln]
  (when (map? vuln)
    (let [aliases (get vuln :aliases [])
          cve-id (first (filter #(str/starts-with? % "CVE-") aliases))
          ghsa-id (:id vuln)
          severity (get-in vuln [:database_specific :severity])
          fixed-versions (->> (get vuln :affected [])
                              (mapcat (fn [affected]
                                        (->> (get affected :ranges [])
                                             (mapcat (fn [r]
                                                       (->> (get r :events [])
                                                            (keep :fixed)))))))
                              (distinct)
                              (vec))]
      {:id          (or cve-id ghsa-id (:id vuln))
       :cve         cve-id
       :ghsa        ghsa-id
       :summary     (:summary vuln)
       :severity    severity
       :fixed-in    fixed-versions
       :published   (:published vuln)
       :references  (->> (get vuln :references [])
                         (filter #(= "ADVISORY" (:type %)))
                         (mapv :url))})))

(defn severity-rank
  "Numeric rank for severity. Higher = worse. Total: 0 for unknown."
  [severity]
  (case (some-> severity str/upper-case)
    "CRITICAL" 4
    "HIGH"     3
    "MODERATE" 2
    "MEDIUM"   2
    "LOW"      1
    0))

(defn sort-vulns-by-severity
  "Sort vulnerabilities by severity (worst first). Pure."
  [vulns]
  (sort-by (comp - severity-rank :severity) vulns))
