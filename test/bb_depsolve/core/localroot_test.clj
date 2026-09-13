(ns bb-depsolve.core.localroot-test
  "A :local/root hit is only a violation when it is both machine-specific and
   committed, and the repair depends on whether it sits in :deps or in an alias."
  (:require [babashka.fs :as fs]
            [bb-depsolve.core.localroot :as lr]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private deps-edn
  "{:paths [\"src\"]
 :deps
 {org.clojure/clojure {:mvn/version \"1.12.5\"}
  acme/vendored {:local/root \"lib/vendored.jar\"}}

 :aliases
 {:build {:deps {acme/build {:mvn/version \"0.1.0\"}}
          :ns-default build.api}

  ;; Sibling working trees, for co-development.
  ;; Second comment line rides along.
  :local-src
  {:extra-paths [\"../acme-schemas/synth\"]
   :override-deps
   {acme/one {:local/root \"../one\"}
    acme/two {:local/root \"../two\"}}}

  :test {:extra-paths [\"test\"]}}}
")

(deftest scan-separates-top-level-deps-from-alias-overrides
  (let [hits (lr/scan deps-edn)
        by   (group-by #(:kind (:scope %)) hits)]
    (is (= 3 (count hits)))
    (is (= ['acme/vendored] (map :lib (:deps by))))
    (testing "an alias hit carries the alias and the key it sat under"
      (is (= [{:lib 'acme/one :path "../one" :scope {:kind :alias :alias :local-src :key :override-deps}}
              {:lib 'acme/two :path "../two" :scope {:kind :alias :alias :local-src :key :override-deps}}]
             (vec (:alias by)))))
    (testing "a coordinate with no :local/root is not a hit"
      (is (not-any? #(= 'acme/build (:lib %)) hits)))))

(deftest scan-tolerates-a-file-that-is-not-a-map
  (is (= [] (lr/scan ";; just a comment\n")))
  (is (= [] (lr/scan "{:deps {unclosed"))))

(deftest classify-tells-the-three-cases-apart
  (let [tmp  (fs/create-temp-dir)
        repo (fs/path tmp "project")
        _    (fs/create-dirs (fs/path repo ".git"))
        _    (fs/create-dirs (fs/path repo "lib"))
        _    (fs/create-dirs (fs/path tmp "sibling"))
        dep  (str (fs/path repo "deps.edn"))]
    (spit dep deps-edn)
    (testing "a path inside the project's own repo ships with every clone"
      (is (= :in-repo (lr/classify {:path "lib"} dep))))
    (testing "a path outside it exists only on this machine"
      (is (= :violation (lr/classify {:path "../sibling"} dep))))
    (testing "a dep file in no git repo commits nothing"
      (let [loose (str (fs/path tmp "loose.deps.edn"))]
        (spit loose deps-edn)
        (is (= :no-repo (lr/classify {:path "../sibling"} loose)))))))

(deftest extract-alias-takes-the-entry-and-its-comments-and-nothing-else
  (let [{:keys [content source]} (lr/extract-alias deps-edn :local-src)]
    (testing "the alias and the comment lines that introduce it are gone"
      (is (not (str/includes? content ":local-src")))
      (is (not (str/includes? content "Sibling working trees")))
      (is (not (str/includes? content "Second comment line"))))
    (testing "every other entry survives byte for byte"
      (is (str/includes? content ":build {:deps {acme/build {:mvn/version \"0.1.0\"}}\n          :ns-default build.api}"))
      (is (str/includes? content ":test {:extra-paths [\"test\"]}"))
      (is (str/includes? content "acme/vendored {:local/root \"lib/vendored.jar\"}")))
    (testing "what is left still parses, with the alias really removed"
      (let [parsed (edn/read-string content)]
        (is (= #{:build :test} (set (keys (:aliases parsed)))))))
    (testing "the extracted source carries the comments and re-reads as the entry"
      (is (str/includes? source "Sibling working trees"))
      (let [round (edn/read-string (str "{" source "}"))]
        (is (= "../one" (get-in round [:local-src :override-deps 'acme/one :local/root])))
        (is (= ["../acme-schemas/synth"] (get-in round [:local-src :extra-paths])))))
    (testing "asking for an alias that is not there says so"
      (is (nil? (lr/extract-alias deps-edn :nope))))))

(deftest splice-alias-merges-into-whatever-local-deps-edn-already-holds
  (let [{:keys [source]} (lr/extract-alias deps-edn :local-src)]
    (testing "no file yet"
      (let [out (lr/splice-alias nil source)]
        (is (str/starts-with? out "{"))
        (is (= "../one" (get-in (edn/read-string out)
                                [:aliases :local-src :override-deps 'acme/one :local/root])))))
    (testing "a file with top-level :deps and a trailing header keeps both"
      (let [existing "{:deps\n {acme/three {:local/root \"../three\"}}}\n\n;; header stays below the map\n"
            out      (lr/splice-alias existing source)
            parsed   (edn/read-string out)]
        (is (= "../three" (get-in parsed [:deps 'acme/three :local/root])))
        (is (= "../two" (get-in parsed [:aliases :local-src :override-deps 'acme/two :local/root])))
        (is (str/includes? out ";; header stays below the map"))
        (is (str/starts-with? out "{"))))
    (testing "a file that already has :aliases gains one, keeping the others"
      (let [existing "{:aliases\n {:local-raster\n  {:override-deps {org/raster {:local/root \"/tmp/raster\"}}}}}\n"
            out      (lr/splice-alias existing source)
            parsed   (edn/read-string out)]
        (is (= #{:local-raster :local-src} (set (keys (:aliases parsed)))))
        (is (= "/tmp/raster" (get-in parsed [:aliases :local-raster :override-deps 'org/raster :local/root])))))))

(deftest move-round-trip-leaves-no-violation-and-loses-no-override
  (let [{:keys [content source]} (lr/extract-alias deps-edn :local-src)
        local (lr/splice-alias nil source)]
    (testing "deps.edn keeps only the in-repo hit, which lint does not flag"
      (is (= [{:lib 'acme/vendored :path "lib/vendored.jar" :scope {:kind :deps}}]
             (lr/scan content))))
    (testing "both overrides survive the move"
      (is (= #{'acme/one 'acme/two} (set (map :lib (lr/scan local))))))))
