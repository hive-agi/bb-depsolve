(ns bb-depsolve.version.rewrite-property-test
  "Property tests for dependency rewriting."
  (:require [bb-depsolve.version.api :as v]
            [bb-depsolve.version.generators :as g]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]))

(defspec p7-update-git-dep-roundtrip 200
  (prop/for-all [{:keys [content lib]} g/gen-git-dep-content
                 new-tag g/gen-semver-tag
                 new-sha g/gen-sha-short]
                (let [updated (v/update-git-dep content lib new-tag new-sha)
                      deps (v/find-git-deps updated)]
                  (and (= 1 (count deps))
                       (= new-tag (:tag (first deps)))
                       (= new-sha (:sha (first deps)))))))

(defspec p7-update-mvn-dep-roundtrip 200
  (prop/for-all [{:keys [content lib]} g/gen-mvn-dep-content
                 new-version g/gen-version-string]
                (let [updated (v/update-mvn-dep content lib new-version)
                      deps (v/find-mvn-deps updated)]
                  (and (= 1 (count deps))
                       (= new-version (:version (first deps)))))))

(defspec p8-update-git-dep-idempotent 200
  (prop/for-all [{:keys [content lib tag sha]} g/gen-git-dep-content]
                (= content (v/update-git-dep content lib tag sha))))

(defspec p8-update-mvn-dep-idempotent 200
  (prop/for-all [{:keys [content lib version]} g/gen-mvn-dep-content]
                (= content (v/update-mvn-dep content lib version))))

(defspec p9-sha-matches-symmetric 200
  (prop/for-all [a g/gen-sha-short
                 b g/gen-sha-full]
                (= (boolean (v/sha-matches? a b))
                   (boolean (v/sha-matches? b a)))))

(defspec p9-sha-matches-reflexive 200
  (prop/for-all [sha g/gen-sha-short]
                (true? (v/sha-matches? sha sha))))

(defspec p13-replace-local-with-git-roundtrip 200
  (prop/for-all [{:keys [content lib]} g/gen-local-dep-content
                 new-tag g/gen-semver-tag
                 new-sha g/gen-sha-short]
                (let [updated (v/replace-local-with-git content lib new-tag new-sha)
                      git-deps (v/find-git-deps updated)
                      local-deps (v/find-local-deps updated)]
                  (and (= 1 (count git-deps))
                       (= new-tag (:tag (first git-deps)))
                       (= new-sha (:sha (first git-deps)))
                       (empty? local-deps)))))

(defspec p14-replace-local-with-mvn-roundtrip 200
  (prop/for-all [{:keys [content lib]} g/gen-local-dep-content
                 new-version g/gen-version-string]
                (let [updated (v/replace-local-with-mvn content lib new-version)
                      mvn-deps (v/find-mvn-deps updated)
                      local-deps (v/find-local-deps updated)]
                  (and (= 1 (count mvn-deps))
                       (= new-version (:version (first mvn-deps)))
                       (empty? local-deps)))))
