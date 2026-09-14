(ns bb-depsolve.core.sync-row-trifecta-test
  "Golden + property + mutation coverage for mismatch-row.

   The mismatch-row pure function is the stratum that maps a schema-validated
   change map into display cells. Its contract is the only way the sync table
   presents a given change, so every coord, every path edge case, and every
   column value must be pinned."
  (:require [bb-depsolve.core.sync :as sync]
            [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

;; =============================================================================
;; Fixtures — change maps
;; =============================================================================

(def ^:private git-deps-edn
  {:coord :git :project "bb-depsolve" :lib 'io.github.hive-agi/hive-test
   :old-tag "v0.4.3" :old-sha "aaaaaaa1a" :new-tag "v0.4.4" :new-sha "bbbbbb1b"
   :path "/w/p/deps.edn"})

(def ^:private git-bb-edn
  {:coord :git :project "bb-depsolve" :lib 'io.github.hive-agi/hive-test
   :old-tag "v0.4.3" :old-sha "aaaaaaa1a" :new-tag "v0.4.4" :new-sha "bbbbbb1b"
   :path "/w/p/bb.edn"})

(def ^:private mvn-with-source-and-unreachable
  {:coord :mvn :project "bb-depsolve" :lib 'io.github.hive-agi/hive-dsl
   :old-version "0.5.23" :new-version "0.5.24"
   :source :clojars
   :unreachable [{:id :central :version "0.5.25"}]
   :path "/w/p/deps.edn"})

(def ^:private mvn-without-source
  {:coord :mvn :project "bb-depsolve" :lib 'io.github.hive-agi/hive-dsl
   :old-version "0.5.23" :new-version "0.5.24"
   :source nil :unreachable nil
   :path "/w/p/deps.edn"})

(def ^:private nil-path
  {:coord :git :project "bb-depsolve" :lib 'io.github.hive-agi/hive-test
   :old-tag "v0.4.3" :old-sha "aaaaaaa" :new-tag "v0.4.4" :new-sha "bbbbbbb"
   :path nil})

(def ^:private blank-path
  {:coord :git :project "bb-depsolve" :lib 'io.github.hive-agi/hive-test
   :old-tag "v0.4.3" :old-sha "aaaaaaa" :new-tag "v0.4.4" :new-sha "bbbbbbb"
   :path ""})

(def ^:private nested-deps-edn
  {:coord :git :project "bb-depsolve" :lib 'io.github.hive-agi/hive-test
   :old-tag "v0.4.3" :old-sha "aaaaaaa1a" :new-tag "v0.4.4" :new-sha "bbbbbb1b"
   :path "/w/p/sub/deps.edn"})

;; =============================================================================
;; Generator — random change maps of both coords
;; =============================================================================

(def ^:private gen-coord
  (gen/elements [:git :mvn]))

(def ^:private gen-path
  (gen/one-of
   [(gen/return nil)
    (gen/return "")
    (gen/return "/w/p/deps.edn")
    (gen/return "/w/p/bb.edn")
    (gen/return "/w/p/sub/deps.edn")
    (gen/return "/a/b/c/d.edn")
    (gen/return "flat.edn")]))

(def ^:private gen-change
  (gen/let [coord gen-coord
            path gen-path
            project gen/string-alphanumeric
            lib gen/string-alphanumeric
            old gen/string-alphanumeric
            new gen/string-alphanumeric]
    (cond-> {:coord coord :project project :lib (symbol lib)
             :path path}
      (= coord :git)
      (assoc :old-tag old :old-sha (apply str (repeatedly 7 #(rand-nth "abcdef0123456789")))
             :new-tag new :new-sha (apply str (repeatedly 7 #(rand-nth "abcdef0123456789"))))
      (= coord :mvn)
      (assoc :old-version old :new-version new
             :source (rand-nth [nil :clojars :maven-central])
             :unreachable (when (rand-nth [true false])
                            [{:id :central :version "99.0.0"}])))))

;; =============================================================================
;; Predicate
;; =============================================================================

(defn- mismatch-row-valid?
  "Every value is a plain string, no ANSI escapes, and :dep-file is either
   `-` or contains no `/`."
  [row]
  (and (map? row)
       (every? (fn [k] (contains? row k)) [:project :dep-file :lib :from :to :note])
       (every? string? (vals row))
       (not-any? #(str/includes? % "\u001b") (vals row))
       (or (= "-" (:dep-file row))
           (not (str/includes? (:dep-file row) "/")))))

;; =============================================================================
;; Mutations
;; =============================================================================

(defn- dep-file-always-dash
  "Broken: dep-file-label always returns `-`, so real paths never appear."
  [change]
  (let [row (sync/mismatch-row change)]
    (assoc row :dep-file "-")))

(defn- dep-file-is-full-path
  "Broken: uses the full path instead of just the file name."
  [change]
  (let [row (sync/mismatch-row change)]
    (assoc row :dep-file (or (:path change) "-"))))

(defn- mvn-rendered-as-git
  "Broken: Maven changes use the :default (git) dispatch instead of :mvn."
  [change]
  (let [default-cells (sync/row-cells (assoc change :coord :does-not-exist))]
    (assoc (sync/mismatch-row change)
           :from (:from default-cells)
           :to (:to default-cells)
           :note (:note default-cells))))

(defn- git-drops-short-sha
  "Broken: git rows omit the SHA column entirely."
  [change]
  (if (= :git (:coord change))
    (let [row (sync/mismatch-row change)]
      (assoc row
             :from (str (:old-tag change) " ")
             :to (str (:new-tag change) " ")))
    (sync/mismatch-row change)))

;; =============================================================================
;; Trifecta
;; =============================================================================

(tri/deftrifecta mismatch-row-trifecta
  sync/mismatch-row
  {:golden-path "test/golden/bb-depsolve/sync-mismatch-rows.edn"
   :cases       {:git-deps-edn              git-deps-edn
                 :git-bb-edn                git-bb-edn
                 :mvn-with-source-unreachable mvn-with-source-and-unreachable
                 :mvn-without-source        mvn-without-source
                 :nil-path                  nil-path
                 :blank-path                blank-path
                 :nested-deps-edn           nested-deps-edn}
   :gen         gen-change
   :pred        mismatch-row-valid?
   :num-tests   200
   :mutations   [["dep-file-always-dash"       dep-file-always-dash]
                 ["dep-file-is-full-path"       dep-file-is-full-path]
                 ["mvn-rendered-as-git"         mvn-rendered-as-git]
                 ["git-drops-short-sha"         git-drops-short-sha]]})
