(ns bb-depsolve.mutation-test
  "Mutation tests for bb-depsolve.version.api critical pure functions.

   Verifies that unit tests catch common classes of bugs by running
   assertions against intentionally broken (mutant) implementations.
   If a mutant survives, the test suite has a blind spot.

   Uses hive-test.mutation macros: Phase 1 runs with real impl (must pass),
   Phase 2 rebinds var to mutant (must fail)."
  (:require [bb-depsolve.version.api :as v]
            [clojure.test :refer [is]]
            [hive-test.mutation :as mut]))

;; =============================================================================
;; M1: version-newer? — the core comparison function
;; =============================================================================

(mut/deftest-mutations version-newer?-mutations-caught
  bb-depsolve.version.api/version-newer?
  [["always-true"
    (fn [_old _new] true)]

   ["always-false"
    (fn [_old _new] false)]

   ["reversed-comparison"
    (fn [old-v new-v]
      ;; Swapped: reports old > new as "newer" (backwards)
      (let [old-parts (v/parse-version-segments old-v)
            new-parts (v/parse-version-segments new-v)
            max-len   (max (count old-parts) (count new-parts))
            pad       (fn [v] (vec (concat v (repeat (- max-len (count v)) 0))))]
        (pos? (compare (pad old-parts) (pad new-parts)))))]

   ["off-by-one"
    (fn [old-v new-v]
      ;; Uses >= instead of > (returns true for equal versions)
      (let [old-parts (v/parse-version-segments old-v)
            new-parts (v/parse-version-segments new-v)
            max-len   (max (count old-parts) (count new-parts))
            pad       (fn [v] (vec (concat v (repeat (- max-len (count v)) 0))))]
        (not (neg? (compare (pad new-parts) (pad old-parts))))))]]

  (fn []
    ;; Strictly newer — catches always-false
    (is (true? (v/version-newer? "1.0.0" "1.0.1")))
    (is (true? (v/version-newer? "1.0.0" "2.0.0")))
    (is (true? (v/version-newer? "0.5.22" "0.5.30")))
    ;; Equal — catches always-true and off-by-one
    (is (false? (v/version-newer? "1.0.0" "1.0.0")))
    (is (false? (v/version-newer? "5.13.0" "5.13.0")))
    ;; Older — catches always-true and reversed-comparison
    (is (false? (v/version-newer? "2.0.0" "1.0.0")))
    (is (false? (v/version-newer? "1.1.0" "1.0.0")))
    (is (false? (v/version-newer? "5.13.0" "5.12.0")))))

;; =============================================================================
;; M2: parse-semver — version string parser
;; =============================================================================

(mut/deftest-mutations parse-semver-mutations-caught
  bb-depsolve.version.api/parse-semver
  [["swap-major-minor"
    (fn [tag]
      ;; Swaps major and minor components
      (when (string? tag)
        (when-let [[_ major minor patch] (re-matches #"v?(\d+)\.(\d+)\.(\d+).*" tag)]
          [(parse-long minor) (parse-long major) (parse-long patch)])))]

   ["always-nil"
    (fn [_tag] nil)]

   ["drop-patch"
    (fn [tag]
      ;; Always returns 0 for patch component
      (when (string? tag)
        (when-let [[_ major minor _patch] (re-matches #"v?(\d+)\.(\d+)\.(\d+).*" tag)]
          [(parse-long major) (parse-long minor) 0])))]]

  (fn []
    ;; Asymmetric major/minor — catches swap-major-minor
    (is (= [1 2 3] (v/parse-semver "v1.2.3")))
    (is (= [3 1 4] (v/parse-semver "v3.1.4")))
    ;; Valid parse — catches always-nil
    (is (some? (v/parse-semver "v1.0.0")))
    (is (= [0 4 0] (v/parse-semver "v0.4.0")))
    ;; Non-zero patch — catches drop-patch
    (is (= [1 2 3] (v/parse-semver "v1.2.3")))
    (is (= [0 0 7] (v/parse-semver "0.0.7")))
    ;; Non-semver still nil
    (is (nil? (v/parse-semver "latest")))
    (is (nil? (v/parse-semver "")))))

;; =============================================================================
;; M3: find-conflicts — transitive dependency conflict detection
;; =============================================================================

(mut/deftest-mutations find-conflicts-mutations-caught
  bb-depsolve.version.api/find-conflicts
  [["always-empty"
    (fn [_trees] {})]

   ["include-single-versions"
    (fn [trees]
      ;; Returns ALL libs (including non-conflicting single-version ones)
      (let [versions (atom {})]
        (letfn [(walk [nodes]
                  (doseq [{:keys [lib version children]} nodes]
                    (swap! versions update lib (fnil conj #{}) version)
                    (walk children)))]
          (walk trees))
        @versions))]

   ["count-wrong"
    (fn [trees]
      ;; Threshold 3+ instead of 2+ — misses 2-version conflicts
      (let [versions (atom {})]
        (letfn [(walk [nodes]
                  (doseq [{:keys [lib version children]} nodes]
                    (swap! versions update lib (fnil conj #{}) version)
                    (walk children)))]
          (walk trees))
        (->> @versions
             (filter (fn [[_ vs]] (> (count vs) 2)))
             (into {}))))]]

  (fn []
    (let [tree [{:lib 'a/a :version "1.0" :children
                 [{:lib 'c/c :version "1.0" :children []}]}
                {:lib 'b/b :version "2.0" :children
                 [{:lib 'c/c :version "2.0" :children []}]}]
          conflicts (v/find-conflicts tree)]
      ;; c/c has 2 versions — catches always-empty and count-wrong
      (is (= 1 (count conflicts)))
      (is (= #{"1.0" "2.0"} (get conflicts 'c/c)))
      ;; Single-version libs must NOT appear — catches include-single-versions
      (is (nil? (get conflicts 'a/a)))
      (is (nil? (get conflicts 'b/b))))
    ;; Empty tree — no conflicts
    (is (empty? (v/find-conflicts [])))))

;; =============================================================================
;; M4: maven-property? — Maven placeholder detection
;; =============================================================================

(mut/deftest-mutations maven-property?-mutations-caught
  bb-depsolve.version.api/maven-property?
  [["always-true"
    (fn [_s] true)]

   ["always-false"
    (fn [_s] false)]]

  (fn []
    ;; Actual properties — catches always-false
    (is (true? (v/maven-property? "${clojure.version}")))
    (is (true? (v/maven-property? "${project.version}")))
    (is (true? (v/maven-property? "${jackson.version}")))
    ;; Normal versions — catches always-true
    (is (false? (v/maven-property? "1.0.0")))
    (is (false? (v/maven-property? "2.17.0")))
    (is (false? (v/maven-property? "v0.4.0")))
    ;; Edge cases
    (is (false? (v/maven-property? nil)))
    (is (false? (v/maven-property? "")))
    (is (false? (v/maven-property? "${}")))))
