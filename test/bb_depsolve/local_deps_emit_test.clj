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

(deftest alias-declared-libs-emit-into-an-override-alias
  (testing "a lib declared under an alias is overridden via :local-overrides :override-deps,
            because a root :deps entry loses to the alias's :extra-deps"
    (let [out (emit [{:lib 'io.github.hive-agi/hive-dsl :path "../hive-dsl" :scope {:kind :deps}}
                     {:lib 'io.github.hive-agi/hive-mcp :path "../hive-mcp"
                      :scope {:kind :alias :alias :test :key :extra-deps}}]
                    "hive-agi")
          parsed (edn/read-string out)]
      (is (= {'io.github.hive-agi/hive-dsl {:local/root "../hive-dsl"}} (:deps parsed)))
      (is (= {'io.github.hive-agi/hive-mcp {:local/root "../hive-mcp"}}
             (get-in parsed [:aliases :local-overrides :override-deps])))
      (is (clojure.string/starts-with? out "{")))))

(deftest regeneration-preserves-an-existing-override-alias
  (testing "a hand-written :local-overrides block and other aliases survive regeneration"
    (let [existing "{:deps {a/a {:local/root \"../a\"}}\n :aliases {:local-overrides {:override-deps {io.github.hive-agi/hive-test {:local/root \"../hive-test\"}}}\n           :mine {:extra-paths [\"dev\"]}}}\n;; trailing\n"
          out (emit [{:lib 'io.github.hive-agi/hive-mcp :path "../hive-mcp"
                      :scope {:kind :alias :alias :test :key :extra-deps}}]
                    "hive-agi" nil existing)
          parsed (edn/read-string out)]
      (is (= {'io.github.hive-agi/hive-test {:local/root "../hive-test"}
              'io.github.hive-agi/hive-mcp  {:local/root "../hive-mcp"}}
             (get-in parsed [:aliases :local-overrides :override-deps])))
      (is (= {:extra-paths ["dev"]} (get-in parsed [:aliases :mine])))
      (is (= {'a/a {:local/root "../a"}} (:deps parsed)) "existing root overrides are kept"))))
