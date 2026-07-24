(ns bb-depsolve.loc-test
  "Guards the namespace-size thresholds the SRP split established.

   red flag > 200, hotspot > 300, critical > 500.
   Only `critical` fails the build; `hotspot` prints and passes."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def red-flag 200)
(def hotspot 300)
(def critical 500)

(def guarded-roots ["src" "test"])

(defn line-counts
  "[{:path :lines}] for every .clj under ROOTS, largest first."
  [roots]
  (->> roots
       (mapcat #(fs/glob % "**.clj"))
       (map (fn [p]
              {:path (str p)
               :lines (count (str/split-lines (slurp (str p))))}))
       (sort-by :lines >)
       vec))

(defn over
  [threshold counts]
  (filterv #(> (:lines %) threshold) counts))

(defn- report
  [label entries]
  (str label ":\n"
       (str/join "\n" (map #(format "  %5d  %s" (:lines %) (:path %)) entries))))

(deftest no-namespace-exceeds-the-critical-threshold-test
  (let [counts (line-counts guarded-roots)
        breaches (over critical counts)]
    (is (empty? breaches)
        (report (str "namespaces over " critical " LOC — split them") breaches))))

(deftest hotspots-are-reported-without-failing-test
  (testing "above the hotspot threshold is a warning, not a failure"
    (let [counts (line-counts guarded-roots)
          hot (remove #(> (:lines %) critical) (over hotspot counts))]
      (when (seq hot)
        (println (report (str "LOC hotspots (> " hotspot ")") hot)))
      (is (every? #(<= (:lines %) critical) hot)))))

(deftest the-census-sees-the-source-tree-test
  (testing "a guard that silently matches nothing is not a guard"
    (let [counts (line-counts guarded-roots)]
      (is (< 40 (count counts))
          "expected the glob to find the whole workspace")
      (is (some #(str/ends-with? (:path %) "src/bb_depsolve/version.clj") counts)))))
