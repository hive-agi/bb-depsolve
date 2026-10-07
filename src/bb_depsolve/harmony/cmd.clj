(ns bb-depsolve.harmony.cmd
  "The harmony command: do this workspace's AOT jars agree with its Clojure?"
  (:require [babashka.fs :as fs]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.core.discovery :as discovery]
            [bb-depsolve.harmony.api :as harmony]
            [bb-depsolve.harmony.plan :as plan]
            [bb-depsolve.harmony.store :as store]
            [clojure.string :as str]))

(defn projects-with-clojure
  "[{:project :dir :version}] for every dep file that declares
   org.clojure/clojure — the only projects whose classpath this check can be
   anchored to. Same discovery `upgrade` walks, so an audit covers exactly what
   an upgrade would have moved."
  [{:keys [root skip-dirs depth project]}]
  (let [dep-files (cond->> (discovery/find-dep-files {:root root
                                                      :skip-dirs skip-dirs
                                                      :depth depth})
                    project (filter #(= project (:project %))))]
    (vec (for [{:keys [path] :as dep-file} dep-files
               :let [content (slurp path)]
               {:keys [lib version]} (discovery/extract-mvn-deps dep-file content)
               :when (= lib harmony/clojure-lib)]
           {:project (:project dep-file) :dir (str (fs/parent path)) :version version}))))

(defn- print-report!
  [{:keys [project version target]} {:keys [verdict findings aot-jars reason unreadable] :as report}]
  (printf "  %-24s clojure %-9s %s\n"
          project
          (if (= version target) version (str version " -> " target))
          (case verdict
            :agreed (ui/c :green (format "%d AOT jar(s) agree" aot-jars))
            :broken (ui/c :red (format "%d of %d AOT jar(s) BREAK" (count findings) aot-jars))
            (ui/c :yellow (str "unverified" (when reason (str ": " (name reason)))))))
  (doseq [{:keys [jar missing-count missing]} findings]
    (println (ui/c :dim (format "      %s" (fs/file-name jar))))
    (println (ui/c :dim (format "        wants %d class(es) this Clojure does not ship: %s"
                                missing-count (str/join ", " missing)))))
  (when (seq unreadable)
    (println (ui/c :dim (format "      %d classpath entry(ies) could not be opened" (count unreadable)))))
  (when (= :broken verdict)
    (println (ui/c :dim (str "      " (plan/explain (assoc report :clojure-version target)))))))

(defn harmony-cmd
  "Check every project's AOT-compiled jars against its Clojure coordinate.

   A jar that ships AOT classes carries the exact class names of the Clojure it
   was built against, gensym suffixes included, so moving the Clojure
   coordinate under it is a NoClassDefFoundError at load time. Version numbers
   cannot express that; this reads it out of the bytes.

   With no --clojure-version this is an AUDIT: is the tree as committed
   coherent? With one, it is a REHEARSAL: would moving every project to that
   version stay coherent?

   A project whose classpath or Clojure jar could not be read is reported
   unverified, never as agreeing — a blind read is not a clean bill.

   Flags:
     --root <dir>              workspace root (default .)
     --project <name>          check one project only
     --clojure-version <v>     rehearse this version instead of the declared one
     --no-fail                 report breaks without exiting 1"
  [{:keys [opts]}]
  (let [{:keys [root skip-dirs depth project clojure-version no-fail]
         :or {root "." depth discovery/default-depth}} opts
        skip-set (if skip-dirs
                   (into #{} (str/split skip-dirs #","))
                   discovery/default-skip-dirs)
        store (store/default-store)
        rows (projects-with-clojure {:root root :skip-dirs skip-set
                                     :depth depth :project project})]
    (println (ui/c :bold (if clojure-version
                           (str "AOT harmony if every project moved to Clojure " clojure-version)
                           "AOT harmony as committed")))
    (println)
    (if (empty? rows)
      (println (ui/c :dim "  No project declares org.clojure/clojure."))
      (let [reports (for [{:keys [version] :as row} rows
                          :let [target (or clojure-version version)]]
                      [(assoc row :target target)
                       (harmony/check-project store (:dir row) target)])
            broken (count (filter (comp plan/broken? second) reports))]
        (doseq [[row report] reports] (print-report! row report))
        (println)
        (println (ui/c :dim (format "  %d project(s) checked, %d broken." (count rows) broken)))
        (when (and (pos? broken) (not no-fail))
          (System/exit 1))))))
