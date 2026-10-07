(ns bb-depsolve.harmony.store
  "Boundary adapters for the harmony ports: zip, ~/.m2 + Maven Central, and
   the Clojure CLI. Every effect this subsystem performs is in this namespace."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :as process]
            [bb-depsolve.harmony.port :as port]
            [bb-depsolve.harmony.refs :as refs]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util.zip ZipEntry ZipFile)))

;; =============================================================================
;; IJarReader — zip
;; =============================================================================

(defn- zip-entry-names
  [^ZipFile zf]
  (mapv #(.getName ^ZipEntry %) (enumeration-seq (.entries zf))))

(defrecord ZipJarReader []
  port/IJarReader
  (entry-names [_ jar]
    (try
      (with-open [zf (ZipFile. (io/file jar))] (zip-entry-names zf))
      (catch Exception _ nil)))
  (class-texts [_ jar]
    (try
      (with-open [zf (ZipFile. (io/file jar))]
        ;; Eager: the ZipFile closes with this form, so a lazy seq handed back
        ;; would read from a closed file. The caller reduces it immediately.
        (into []
              (comp (filter refs/class-entry?)
                    (map (fn [n]
                           [n (String. (.readAllBytes (.getInputStream zf (.getEntry zf n)))
                                       "ISO-8859-1")])))
              (zip-entry-names zf)))
      (catch Exception _ nil))))

;; =============================================================================
;; IArtifactResolver — ~/.m2, then Maven Central
;; =============================================================================

(def ^:private central "https://repo1.maven.org/maven2")

(defn m2-path
  "Where ~/.m2 keeps LIB at VERSION, whether or not it is there yet."
  [lib version]
  (let [group (str/replace (or (namespace lib) (name lib)) "." "/")
        artifact (name lib)]
    (str (fs/path (fs/home) ".m2" "repository" group artifact version
                  (str artifact "-" version ".jar")))))

(defn- central-url
  [lib version]
  (let [group (str/replace (or (namespace lib) (name lib)) "." "/")
        artifact (name lib)]
    (str central "/" group "/" artifact "/" version "/" artifact "-" version ".jar")))

(defrecord M2Resolver [fetch?]
  port/IArtifactResolver
  (local-jar [_ lib version]
    (let [path (m2-path lib version)]
      (when (fs/exists? path) path)))
  (resolve-jar [_ lib version]
    (let [path (m2-path lib version)]
      (cond
        (fs/exists? path) path
        (not fetch?) nil
        :else
        (try
          (let [{:keys [status body]} (http/get (central-url lib version)
                                                {:as :bytes :throw false})]
            (when (= 200 status)
              (fs/create-dirs (fs/parent path))
              (io/copy body (io/file path))
              path))
          (catch Exception _ nil))))))

;; =============================================================================
;; IClasspathSource — clojure -Spath
;; =============================================================================

(defrecord ClojureCliClasspath []
  port/IClasspathSource
  (project-jars [_ project-dir]
    (try
      (let [{:keys [exit out]} (process/sh {:dir (str project-dir) :err :inherit}
                                           "clojure" "-Spath")]
        (when (zero? exit)
          (->> (str/split (str/trim out) #":")
               (filter #(str/ends-with? % ".jar"))
               (filterv fs/exists?))))
      (catch Exception _ nil))))

;; =============================================================================
;; Composition
;; =============================================================================

(defn default-store
  "The adapters `bb-depsolve.harmony.api` runs against by default.

   A map of the three roles rather than one object implementing all three: the
   roles are independent, and a test that needs to stub one should not have to
   name the other two."
  ([] (default-store {}))
  ([{:keys [fetch?] :or {fetch? true}}]
   {:reader (->ZipJarReader)
    :resolver (->M2Resolver fetch?)
    :classpath (->ClojureCliClasspath)}))
