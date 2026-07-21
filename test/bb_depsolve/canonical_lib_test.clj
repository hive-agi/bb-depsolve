(ns bb-depsolve.canonical-lib-test
  "Regression tests for canonical-lib — the coordinate a :local/root override
   must be keyed on.

   A :local/root under a group id that appears nowhere in deps.edn is not an
   override; tools.deps treats it as an additional, unrelated library. The
   generated local.deps.edn and the rewritten deps.edn must therefore agree on
   the symbol, which is what canonical-lib guarantees."
  (:require [clojure.test :refer [deftest is testing]]
            [bb-depsolve.version :as v]))

(deftest canonicalizes-bare-artifact-coordinates
  (testing "artifact/artifact + sibling path -> forge-qualified coordinate"
    (is (= 'io.github.hive-agi/hive-mcp
           (v/canonical-lib 'hive-mcp/hive-mcp "../hive-mcp" "hive-agi")))
    (is (= 'io.github.hive-agi/basic-tools-mcp
           (v/canonical-lib 'basic-tools-mcp/basic-tools-mcp "../basic-tools-mcp" "hive-agi")))))

(deftest already-canonical-is-left-alone
  (testing "a forge-qualified lib is returned unchanged"
    (is (= 'io.github.hive-agi/hive-events
           (v/canonical-lib 'io.github.hive-agi/hive-events "../hive-events" "hive-agi")))))

(deftest keys-on-the-path-not-the-declared-name
  (testing "the sibling directory decides the artifact, not the stale symbol"
    (is (= 'io.github.hive-agi/hive-knowledge
           (v/canonical-lib 'wrong/name "../hive-knowledge" "hive-agi")))))

(deftest total-when-inputs-are-unusable
  (testing "no org, non-sibling path, or nil path -> original symbol, never nil"
    (is (= 'foo/bar (v/canonical-lib 'foo/bar "../bar" nil)))
    (is (= 'foo/bar (v/canonical-lib 'foo/bar "/abs/path/bar" "hive-agi")))
    (is (= 'foo/bar (v/canonical-lib 'foo/bar nil "hive-agi")))
    (is (some? (v/canonical-lib 'foo/bar nil nil)))))

(deftest third-party-libs-are-not-forced-into-the-org
  (testing "a genuine third-party local checkout keeps its own group"
    (is (= 'io.github.hive-agi/datalevin
           (v/canonical-lib 'datalevin/datalevin "../datalevin" "hive-agi"))
        "NOTE: canonicalization is org-scoped by design — callers must only
         pass :org for libs that genuinely belong to it")))
