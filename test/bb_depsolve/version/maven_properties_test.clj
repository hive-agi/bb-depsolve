(ns bb-depsolve.version.maven-properties-test
  "Maven property resolution before the unresolved-coord filter. A coord
   dropped for a `${...}` version is a transitive edge no CVE query ever sees,
   so every property a POM can resolve on its own must be resolved."
  (:require [bb-depsolve.version.api :as v]
            [clojure.test :refer [deftest is testing]]))

(def ^:private tika-style-pom
  "A child module in the tika shape: its own groupId/version are inherited
   from <parent>, sibling modules are versioned ${project.version}, and a
   third-party dep is versioned by a <properties> entry that itself refers to
   another property."
  (str "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">"
       "<parent><groupId>org.apache.tika</groupId>"
       "<artifactId>tika-parsers</artifactId><version>2.9.2</version></parent>"
       "<artifactId>tika-parsers-standard-package</artifactId>"
       "<properties><guava.version>32.1.3-jre</guava.version>"
       "<pinned.guava>${guava.version}</pinned.guava></properties>"
       "<dependencies>"
       "<dependency><groupId>${project.groupId}</groupId>"
       "<artifactId>tika-parser-pdf-module</artifactId>"
       "<version>${project.version}</version></dependency>"
       "<dependency><groupId>com.google.guava</groupId><artifactId>guava</artifactId>"
       "<version>${pinned.guava}</version></dependency>"
       "<dependency><groupId>org.slf4j</groupId><artifactId>slf4j-api</artifactId>"
       "<version>${slf4j.version}</version></dependency>"
       "</dependencies></project>"))

(deftest parse-pom-properties-reads-properties-and-project-coords-test
  (let [props (v/parse-pom-properties tika-style-pom)]
    (is (= "32.1.3-jre" (get props "guava.version")))
    (testing "project.* falls back to <parent> when the module declares none"
      (is (= "2.9.2" (get props "project.version")))
      (is (= "org.apache.tika" (get props "project.groupId")))
      (is (= "tika-parsers-standard-package" (get props "project.artifactId")))
      (is (= "2.9.2" (get props "project.parent.version"))))
    (is (= {} (v/parse-pom-properties nil)) "total on nil")
    (is (= {} (v/parse-pom-properties "<<not xml")) "total on garbage")))

(deftest pom-parent-coord-test
  (is (= {:group "org.apache.tika" :artifact "tika-parsers" :version "2.9.2"}
         (v/pom-parent-coord tika-style-pom)))
  (is (nil? (v/pom-parent-coord "<project><artifactId>x</artifactId></project>")))
  (is (nil? (v/pom-parent-coord nil))))

(deftest substitute-properties-test
  (let [props (v/parse-pom-properties tika-style-pom)]
    (is (= {:lib 'org.apache.tika/tika-parser-pdf-module :version "2.9.2"}
           (v/substitute-properties
            {:lib (symbol "${project.groupId}/tika-parser-pdf-module")
             :version "${project.version}"}
            props))
        "a placeholder in the lib's group is substituted too")
    (is (= "32.1.3-jre"
           (:version (v/substitute-properties
                      {:lib 'com.google.guava/guava :version "${pinned.guava}"} props)))
        "a property defined by another property resolves transitively")
    (is (= "${slf4j.version}"
           (:version (v/substitute-properties
                      {:lib 'org.slf4j/slf4j-api :version "${slf4j.version}"} props)))
        "an unknown property is left in place for the filter to report")
    (is (= "${a}"
           (:version (v/substitute-properties {:lib 'x/y :version "${a}"}
                                              {"a" "${b}" "b" "${a}"})))
        "a property cycle terminates and stays unresolved")))

(deftest parse-pom-deps-resolved-keeps-what-the-pom-can-resolve-test
  (is (= [{:lib 'org.apache.tika/tika-parser-pdf-module :version "2.9.2"}
          {:lib 'com.google.guava/guava :version "32.1.3-jre"}
          {:lib 'org.slf4j/slf4j-api :version "${slf4j.version}"}]
         (v/parse-pom-deps-resolved tika-style-pom {}))
      "only slf4j, whose property lives in a parent POM, stays unresolved")
  (is (= "2.0.13"
         (:version (last (v/parse-pom-deps-resolved
                          tika-style-pom {"slf4j.version" "2.0.13"}))))
      "inherited (parent) properties fill what the child leaves open")
  (is (= "32.1.3-jre"
         (:version (second (v/parse-pom-deps-resolved
                            tika-style-pom {"guava.version" "1.0"}))))
      "the child's own properties win over inherited ones"))
