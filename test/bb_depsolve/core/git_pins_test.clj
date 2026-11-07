(ns bb-depsolve.core.git-pins-test
  "A git coordinate's :git/tag and :git/sha must name the same commit
   (friction 20261022203508-38a4ed5d: hive-dsl v0.5.36 pinned 06f66e9 while
   the tag is e6183cb)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [bb-depsolve.core.git-pins :as gp]))

(def ^:private annotated-out
  (str "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\trefs/tags/v0.5.36\n"
       "e6183cb9a1210856cde6534e1f274a37b71e0e4e\trefs/tags/v0.5.36^{}\n"))

(def ^:private lightweight-out
  "80b056398276819bd3833b5588c1fc47c3c7607e\trefs/tags/v0.5.37\n")

(deftest git-url-follows-tools-deps-inference
  (is (= "https://github.com/hive-agi/hive-dsl.git"
         (gp/git-url 'io.github.hive-agi/hive-dsl {})))
  (is (= "https://gitlab.com/org/x.git" (gp/git-url 'com.gitlab.org/x {})))
  (is (= "https://example.com/r.git"
         (gp/git-url 'foo/bar {:git/url "https://example.com/r.git"})))
  (is (nil? (gp/git-url 'foo/bar {}))))

(deftest pins-walks-deps-aliases-and-tasks
  (let [m {:deps    {'io.github.a/x {:git/tag "v1" :git/sha "abc"}
                     'io.github.a/y {:git/sha "def"}
                     'm/n           {:mvn/version "1"}}
           :aliases {:test {:extra-deps {'io.github.a/z {:git/tag "v2" :git/sha "123"}}}}
           :tasks   {'test  {:extra-deps {'io.github.a/w {:git/tag "v3" :git/sha "456"}}}
                     'other '(shell "x")}}]
    (is (= #{'io.github.a/x 'io.github.a/z 'io.github.a/w} (set (map :lib (gp/pins m)))))
    (is (= [:aliases :test :extra-deps]
           (:where (first (filter #(= 'io.github.a/z (:lib %)) (gp/pins m))))))))

(deftest tag-commit-prefers-the-peeled-line
  (is (= "e6183cb9a1210856cde6534e1f274a37b71e0e4e" (gp/tag-commit "v0.5.36" annotated-out)))
  (is (= "80b056398276819bd3833b5588c1fc47c3c7607e" (gp/tag-commit "v0.5.37" lightweight-out)))
  (is (nil? (gp/tag-commit "v9" lightweight-out))))

(deftest verdict-catches-the-v0-5-36-mismatch
  (let [pin {:lib 'io.github.hive-agi/hive-dsl :tag "v0.5.36" :sha "06f66e9"}]
    (testing "the shipped pin disagrees with its tag"
      (is (= :mismatch (:status (gp/verdict pin {:exit 0 :out annotated-out})))))
    (testing "the tag's own short sha agrees"
      (is (= :ok (:status (gp/verdict (assoc pin :sha "e6183cb") {:exit 0 :out annotated-out})))))
    (testing "no tag, or no answer, is not a pass"
      (is (= :missing-tag (:status (gp/verdict pin {:exit 0 :out ""}))))
      (is (= :unreadable (:status (gp/verdict pin {:exit 128 :out "" :err "fatal"}))))
      (is (= :unreadable (:status (gp/verdict pin nil)))))
    (is (str/includes? (gp/describe (gp/verdict pin {:exit 0 :out annotated-out})) "MISMATCH"))))

(deftest check-files-asks-each-url-tag-once
  (let [f     (java.io.File/createTempFile "deps" ".edn")
        calls (atom 0)]
    (spit f (pr-str {:deps    {'io.github.a/x {:git/tag "v0.5.37" :git/sha "80b0563"}}
                     :aliases {:t {:extra-deps {'io.github.a/x {:git/tag "v0.5.37" :git/sha "80b0563"}}}}}))
    (let [vs (gp/check-files! [(str f) "/no/such/file.edn"]
                              (fn [_ _] (swap! calls inc) {:exit 0 :out lightweight-out}))]
      (is (= [:ok :ok] (mapv :status vs)))
      (is (= 1 @calls)))
    (.delete f)))

(deftest this-repo-pins-every-git-coordinate-with-tag-and-sha
  (testing "every tagged git dep in bb.edn / deps.edn names a URL the check can ask"
    (doseq [f ["bb.edn" "deps.edn"]
            p (gp/pins (edn/read-string (slurp f)))]
      (is (some? (:url p)) (str f " " (:lib p))))))
