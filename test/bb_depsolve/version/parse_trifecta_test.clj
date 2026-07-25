(ns bb-depsolve.version.parse-trifecta-test
  "Golden + property + mutation coverage for git tag-listing parsers.

   These parsers decide which SHA a :git/sha pin is rewritten to, so a wrong
   answer is a broken dependency for every consumer. The mutation facet pins
   the specific regression: an implementation that ignores the peeled `^{}`
   commit of an annotated tag."
  (:require [bb-depsolve.version :as v]
            [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :as tri]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private annotated-remote
  (str "1111111111111111111111111111111111111111\trefs/tags/v0.1.0\n"
       "2222222222222222222222222222222222222222\trefs/tags/v0.2.0\n"
       "3333333333333333333333333333333333333333\trefs/tags/v0.2.0^{}\n"))

(def ^:private peeled-first-remote
  "The peeled commit is listed BEFORE its tag object. Line order must not
   decide which SHA wins — only peeling does."
  (str "3333333333333333333333333333333333333333\trefs/tags/v0.2.0^{}\n"
       "2222222222222222222222222222222222222222\trefs/tags/v0.2.0\n"))

(def ^:private lightweight-remote
  (str "4444444444444444444444444444444444444444\trefs/tags/v1.0.0\n"
       "5555555555555555555555555555555555555555\trefs/tags/v1.1.0\n"))

(def ^:private local-mixed
  (str "v0.2.0\t3333333333333333333333333333333333333333\t2222222222222222222222222222222222222222\n"
       "v0.1.0\t\t1111111111111111111111111111111111111111\n"))

(def ^:private gen-tag-output
  "Plausible-to-hostile tag listings: well-formed lines, truncated lines,
   blank lines and free text."
  (gen/let [lines (gen/vector
                   (gen/one-of
                    [(gen/elements (str/split-lines annotated-remote))
                     (gen/elements (str/split-lines local-mixed))
                     (gen/return "")
                     (gen/return "\t")
                     gen/string-alphanumeric])
                   0 12)]
    (str/join "\n" lines)))

(defn- tag-entries?
  "Every entry carries a tag, a sha, and a sha-short derived from it."
  [entries]
  (and (vector? entries)
       (every? (fn [{:keys [tag sha sha-short]}]
                 (and (string? tag) (seq tag)
                      (string? sha) (seq sha)
                      (string? sha-short)
                      (str/starts-with? sha sha-short)))
               entries)))

;; =============================================================================
;; Trifecta — parse-ls-remote-tags
;; =============================================================================

(tri/deftrifecta parse-ls-remote-tags-trifecta
  bb-depsolve.version/parse-ls-remote-tags
  {:golden-path "test/golden/bb-depsolve/parse-ls-remote-tags.edn"
   :cases       {:annotated    annotated-remote
                 :peeled-first peeled-first-remote
                 :lightweight  lightweight-remote
                 :empty        ""
                 :garbage      "not a tag listing at all"}
   :gen         gen-tag-output
   :pred        tag-entries?
   :num-tests   200
   :mutations   [["ignores-the-peeled-commit"
                  (fn [output]
                    (->> (str/split-lines (or output ""))
                         (remove #(str/includes? % "^{}"))
                         (keep (fn [line]
                                 (let [[sha ref] (str/split line #"\t" 2)]
                                   (when (and sha ref)
                                     {:tag (str/replace ref "refs/tags/" "")
                                      :sha sha
                                      :sha-short (subs sha 0 (min 7 (count sha)))}))))
                         vec))]
                 ["lets-line-order-decide-the-sha"
                  (fn [output]
                    (->> (str/split-lines (or output ""))
                         (keep (fn [line]
                                 (let [[sha ref] (str/split line #"\t" 2)]
                                   (when (and (seq sha) ref)
                                     {:tag (-> ref
                                               (str/replace #"^refs/tags/" "")
                                               (str/replace #"\^\{\}$" ""))
                                      :sha sha
                                      :sha-short (subs sha 0 (min 7 (count sha)))}))))
                         (reduce (fn [acc e] (assoc acc (:tag e) e)) {})
                         vals
                         vec))]]})

;; =============================================================================
;; Trifecta — parse-local-tag-output
;; =============================================================================

(tri/deftrifecta parse-local-tag-output-trifecta
  bb-depsolve.version/parse-local-tag-output
  {:golden-path "test/golden/bb-depsolve/parse-local-tag-output.edn"
   :cases       {:mixed local-mixed
                 :empty ""
                 :ragged "v0.1.0\n\t\t\nv0.2.0\t\t"}
   :gen         gen-tag-output
   :pred        tag-entries?
   :num-tests   200
   :mutations   [["prefers-the-tag-object-over-the-commit"
                  (fn [output]
                    (->> (str/split-lines (or output ""))
                         (keep (fn [line]
                                 (let [[tag _peeled direct] (str/split line #"\t" -1)]
                                   (when (and (seq tag) (seq direct))
                                     {:tag tag
                                      :sha direct
                                      :sha-short (subs direct 0 (min 7 (count direct)))}))))
                         vec))]]})
