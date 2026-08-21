(ns bb-depsolve.local-deps-emit-test
  "Regression tests for the local.deps.edn emitter.

   The generated file is consumed as `clj -Sdeps \"$(cat local.deps.edn)\"`, so
   it must (a) start with `{` and (b) READ as EDN. find-local-deps reports one
   entry per :local/root OCCURRENCE, so the same lib arrives once per alias it
   appears in; the emitter must collapse those to one key."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [bb-depsolve.core.lint :as lint]))

(def ^:private emit #'lint/generate-local-deps-edn)

(deftest repeated-occurrences-emit-one-key
  (testing "same lib in :deps and in an alias -> a single, readable entry"
    (let [out (emit [{:lib 'io.github.hive-agi/hive-dsl  :path "../hive-dsl"}
                     {:lib 'io.github.hive-agi/hive-test :path "../hive-test"}
                     {:lib 'io.github.hive-agi/hive-test :path "../hive-test"}]
                    "hive-agi")
          parsed (edn/read-string out)]
      (is (= {'io.github.hive-agi/hive-dsl  {:local/root "../hive-dsl"}
              'io.github.hive-agi/hive-test {:local/root "../hive-test"}}
             (:deps parsed))))))

(deftest first-occurrence-wins
  (testing "a repeated lib keeps the path of its first occurrence"
    (let [out (emit [{:lib 'io.github.hive-agi/hive-dsl :path "../hive-dsl"}
                     {:lib 'io.github.hive-agi/hive-dsl :path "/elsewhere/hive-dsl"}]
                    "hive-agi")]
      (is (= {'io.github.hive-agi/hive-dsl {:local/root "../hive-dsl"}}
             (:deps (edn/read-string out)))))))

(deftest duplicates-after-canonicalization-also-collapse
  (testing "two spellings that canonicalize to one coordinate yield one key"
    (let [out (emit [{:lib 'hive-mcp/hive-mcp :path "../hive-mcp"}
                     {:lib 'io.github.hive-agi/hive-mcp :path "../hive-mcp"}]
                    "hive-agi")]
      (is (= {'io.github.hive-agi/hive-mcp {:local/root "../hive-mcp"}}
             (:deps (edn/read-string out)))))))

(deftest emitted-text-starts-with-the-map
  (testing "comments stay below the map — -Sdeps reads a leading ;; as a file path"
    (let [out (emit [{:lib 'io.github.hive-agi/hive-dsl :path "../hive-dsl"}] "hive-agi")]
      (is (clojure.string/starts-with? out "{"))
      (is (clojure.string/includes? out ";; local.deps.edn")))))
