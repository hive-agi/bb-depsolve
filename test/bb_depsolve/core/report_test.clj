(ns bb-depsolve.core.report-test
  "Tests for the report command's dependency matrix over a throwaway workspace.
   Output is read through `with-out-str`; the non-TTY table path renders it."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.report :as report]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- workspace
  "Write each [project file content] triple under a fresh root. => root path"
  [files]
  (let [root (str (fs/create-temp-dir {:prefix "bb-depsolve-report"}))]
    (doseq [[project fname content] files
            :let [f (fs/path root project fname)]]
      (fs/create-dirs (fs/parent f))
      (spit (str f) content))
    root))

(defn- strip-ansi [s] (str/replace s #"\[[0-9;]*m" ""))

(defn- run [root & {:as opts}]
  (strip-ansi (with-out-str (report/report-cmd {:opts (merge {:root root} opts)}))))

(deftest report-counts-shared-libraries-and-drift-test
  (let [root (workspace
              [["alpha" "deps.edn" (str "{:deps {shared/drifts {:mvn/version \"1.0.0\"}\n"
                                        "        shared/agrees {:mvn/version \"3.0.0\"}\n"
                                        "        only/alpha {:mvn/version \"9.9.9\"}}}")]
               ["beta" "deps.edn" (str "{:deps {shared/drifts {:mvn/version \"2.0.0\"}\n"
                                       "        shared/agrees {:mvn/version \"3.0.0\"}}}")]])
        out (run root)]
    (is (str/includes? out "2 libraries shared, 1 with version drift"))
    (testing "the matrix lists shared libraries only"
      (is (str/includes? out "shared/drifts"))
      (is (str/includes? out "shared/agrees"))
      (is (not (str/includes? out "only/alpha"))
          "a library one project uses is not part of the matrix"))
    (testing "each project is a column carrying its own version"
      (let [row (first (filter #(str/starts-with? % "shared/drifts") (str/split-lines out)))]
        (is (str/includes? row "1.0.0"))
        (is (str/includes? row "2.0.0"))))))

(deftest report-includes-bb-edn-and-shadow-cljs-test
  (let [root (workspace
              [["alpha" "bb.edn" "{:deps {shared/lib {:mvn/version \"1.0.0\"}}}"]
               ["ui" "shadow-cljs.edn" "{:dependencies [[shared/lib \"1.0.0\"]]}"]])
        out (run root)]
    (is (str/includes? out "1 libraries shared, 0 with version drift"))
    (is (str/includes? out "shared/lib"))))

(deftest report-honours-skip-dirs-test
  (let [root (workspace
              [["alpha" "deps.edn" "{:deps {shared/lib {:mvn/version \"1.0.0\"}}}"]
               ["beta" "deps.edn" "{:deps {shared/lib {:mvn/version \"2.0.0\"}}}"]])]
    (is (str/includes? (run root) "1 libraries shared, 1 with version drift"))
    (is (str/includes? (run root :skip-dirs "beta") "0 libraries shared, 0 with version drift")
        "a skipped project contributes no column")))
