(ns bb-depsolve.core.lint-test
  "Tests for lint --fix output: the emitted local.deps.edn must satisfy the
   usage line it documents."
  (:require [bb-depsolve.core.lint :as lint]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private entries
  [{:lib 'io.github.hive-agi/hive-spi  :path "../hive-spi"}
   {:lib 'io.github.hive-agi/hive-test :path "../hive-test"}])

(deftest generated-file-starts-with-the-map
  (testing "the clj CLI reads a -Sdeps argument not starting with { as a FILE PATH,
            so a leading comment breaks the very command the header documents"
    (let [out (#'lint/generate-local-deps-edn entries "hive-agi")]
      (is (str/starts-with? out "{")
          "leading ;; lines make `clj -Sdeps \"$(cat local.deps.edn)\"` resolve as a path")
      (is (str/includes? out ";; local.deps.edn")
          "the header is kept, just moved below the map"))))

(deftest generated-file-is-readable-edn-with-every-entry
  (let [out    (#'lint/generate-local-deps-edn entries "hive-agi")
        parsed (edn/read-string out)]
    (is (= 2 (count (:deps parsed))))
    (is (every? #(contains? % :local/root) (vals (:deps parsed))))
    (testing "trailing comments do not disturb the parse"
      (is (= "../hive-spi"
             (some (fn [[k v]] (when (= "hive-spi" (name k)) (:local/root v)))
                   (:deps parsed)))))))
