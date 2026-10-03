(ns bb-depsolve.canonical-lib-test
  "Regression tests for canonical-lib — the coordinate a :local/root override
   must be keyed on.

   A :local/root under a group id that appears nowhere in deps.edn is not an
   override; tools.deps treats it as an additional, unrelated library. The
   generated local.deps.edn and the rewritten deps.edn must therefore agree on
   the symbol, which is what canonical-lib guarantees."
  (:require [clojure.test :refer [deftest is testing]]
            [bb-depsolve.version.api :as v]))

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

(deftest declared-coordinate-beats-the-directory-guess
  (testing "a vendored clone under ../clones-ref/ keeps the group deps.edn declares"
    (is (= 'datalevin/datalevin
           (v/canonical-lib 'datalevin/datalevin "../clones-ref/datalevin" "hive-agi"
                            #{'datalevin/datalevin 'org.clojure/clojure}))
        "the project declares datalevin/datalevin; io.github.hive-agi/datalevin is a phantom"))
  (testing "a bare spelling resolves to the declared coordinate with the same artifact"
    (is (= 'io.github.hive-agi/hive-mcp
           (v/canonical-lib 'hive-mcp/hive-mcp "../hive-mcp" "hive-agi"
                            #{'io.github.hive-agi/hive-mcp}))))
  (testing "the path's directory finds the declared coordinate when the symbol is stale"
    (is (= 'acme/hive-knowledge
           (v/canonical-lib 'wrong/name "../hive-knowledge" "hive-agi"
                            #{'acme/hive-knowledge}))))
  (testing "nothing declared under that name -> the org guess is still the fallback"
    (is (= 'io.github.hive-agi/hive-mcp
           (v/canonical-lib 'hive-mcp/hive-mcp "../hive-mcp" "hive-agi"
                            #{'org.clojure/clojure})))
    (is (= 'io.github.hive-agi/hive-mcp
           (v/canonical-lib 'hive-mcp/hive-mcp "../hive-mcp" "hive-agi" nil))))
  (testing "two declared coordinates sharing the artifact are ambiguous -> no pick"
    (is (= 'io.github.hive-agi/x
           (v/canonical-lib 'x/x "../x" "hive-agi" #{'a/x 'b/x})))))

(deftest declared-libs-reads-the-non-local-declarations
  (testing "root :deps and every alias dep map count; :local/root occurrences do not"
    (is (= #{'datalevin/datalevin 'io.github.hive-agi/hive-mcp 'org.clojure/test.check}
           (v/declared-libs
            "{:deps {datalevin/datalevin {:mvn/version \"1.1.0\"}
                     hive-mcp/hive-mcp {:local/root \"../hive-mcp\"}}
              :aliases {:test {:extra-deps {io.github.hive-agi/hive-mcp {:git/tag \"v1\" :git/sha \"abc\"}}}
                        :dev {:override-deps {org.clojure/test.check {:mvn/version \"1\"}}}
                        :run {:main-opts [\"-m\" \"x\"]}}}"))))
  (testing "unreadable content declares nothing"
    (is (= #{} (v/declared-libs "{:deps")))
    (is (= #{} (v/declared-libs nil)))))

(deftest third-party-libs-are-not-forced-into-the-org
  (testing "a genuine third-party local checkout keeps its own group"
    (is (= 'io.github.hive-agi/datalevin
           (v/canonical-lib 'datalevin/datalevin "../datalevin" "hive-agi"))
        "NOTE: canonicalization is org-scoped by design — callers must only
         pass :org for libs that genuinely belong to it")))
