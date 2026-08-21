(ns bb-depsolve.version.vuln
  "OSV vulnerability parsing and severity ordering. Pure."
  (:require [clojure.string :as str]
            [bb-depsolve.version.semver :as semver]))

(defn parse-osv-vuln
  "Parse a single OSV vulnerability entry into a normalized map.
   Total: returns nil for unparseable input.

   :severity is OSV's qualitative label (database_specific); :cvss is the
   quantitative CVSS vector from the schema's `severity` array, which the
   label does not carry. Both may be absent."
  [vuln]
  (when (map? vuln)
    (let [aliases (get vuln :aliases [])
          cve-id (first (filter #(str/starts-with? % "CVE-") aliases))
          ghsa-id (:id vuln)
          severity (get-in vuln [:database_specific :severity])
          cvss (->> (get vuln :severity [])
                    (filter #(str/starts-with? (or (:type %) "") "CVSS"))
                    (map :score)
                    (remove str/blank?)
                    (first))
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
       :cvss        cvss
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

(defn recommended-fix
  "The lowest version that clears EVERY vuln in `vulns` for a dep currently at
   `current`. Pure.

   Per vuln, the candidate is the smallest `fixed` version strictly newer than
   `current`; the recommendation is the LARGEST such candidate, since a version
   that only clears one advisory still ships the others. Returns nil when
   `current` is unparseable, when any vuln publishes no usable fixed version
   (the fix is then not expressible as a version bump), or when nothing is
   newer than what is already pinned."
  [current vulns]
  (when (and (string? current) (seq vulns))
    (let [per-vuln (map (fn [{:keys [fixed-in]}]
                          (->> fixed-in
                               (filter string?)
                               (filter #(semver/version-newer? current %))
                               (sort semver/version-compare)
                               (first)))
                        vulns)]
      (when (every? some? per-vuln)
        (last (sort semver/version-compare per-vuln))))))
