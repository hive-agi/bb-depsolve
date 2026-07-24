(ns bb-depsolve.release.render
  "Terminal and dot rendering for the coordinated-release commands."
  (:require [babashka.fs :as fs]
            [bb-depsolve.cascade :as cas]
            [bb-depsolve.collect :as col]
            [bb-depsolve.graph :as graph]
            [bb-depsolve.ui :as ui]
            [clojure.string :as str]))

(defn print-cycles
  [g]
  (let [cycles (graph/cycles g)
        blocked (graph/blocked g)]
    (when (seq cycles)
      (println)
      (println (ui/c :red (format "%d dependency cycle(s) — these cannot be ordered:" (count cycles))))
      (doseq [c cycles]
        (println (str "  " (ui/c :yellow (str/join " <-> " (sort c)))))))
    (when (seq blocked)
      (println)
      (println (ui/c :yellow (format "%d project(s) blocked behind a cycle:" (count blocked))))
      (println (str "  " (ui/c :dim (str/join " " blocked)))))))

(defn print-unlinked
  [dep-files nodes]
  (let [unlinked (col/collect-unlinked-pins dep-files nodes)]
    (when (seq unlinked)
      (println)
      (println (ui/c :yellow
                     (format "%d internal pin(s) use a bare :git/sha — no version to compare, no edge derived:"
                             (count unlinked))))
      (doseq [{:keys [project dep sha]} (distinct unlinked)]
        (printf "  %-24s -> %-24s %s\n"
                (ui/c :cyan project) dep (ui/c :dim (subs sha 0 (min 7 (count sha)))))))))

(defn graph->dot
  [g]
  (str "digraph deps {\n  rankdir=LR;\n"
       (str/join (for [p (graph/projects g)
                       d (sort (graph/depends-on g p))]
                   (format "  \"%s\" -> \"%s\";\n" p d)))
       "}\n"))

(defn print-plan
  [plan]
  (let [{:keys [mode timeout-ms]} (get-in plan [:policy :await])]
    (println (ui/c :bold (format "Cascade plan — %d release(s) in %d wave(s)"
                                 (count (cas/plan-projects plan))
                                 (count (:waves plan)))))
    (printf "  seeds: %s\n" (ui/c :cyan (str/join " " (sort (:seeds plan)))))
    (printf "  bump:  %s   await: %s\n"
            (ui/c :dim (name (get-in plan [:policy :requested-bump])))
            (ui/c :dim (if (= :wait mode)
                         (format "wait up to %ds per wave" (quot timeout-ms 1000))
                         "skipped (--no-wait)")))
    (when (seq (:unknown-seeds plan))
      (println (ui/c :yellow (format "  unknown seeds ignored: %s"
                                     (str/join " " (:unknown-seeds plan))))))
    (println)
    (doseq [w (:waves plan)]
      (println (ui/c :bold (format "  Wave %d" (:index w))))
      (doseq [{:keys [project role release-mode current-version next-version
                      bump-kind pin-updates version-drift]} (:steps w)]
        (printf "    %-26s %-9s %s\n"
                (ui/c :cyan project)
                (ui/c :dim (name role))
                (if (= :rolling release-mode)
                  (ui/c :dim (format "%s -> rolling (the push mints the version)" current-version))
                  (format "%s -> %s  (%s)"
                          (ui/c :red current-version)
                          (ui/c :green (or next-version "?"))
                          (name bump-kind))))
        (when version-drift
          (println (ui/c :yellow
                         (format "        VERSION file says %s but consumers already pin %s — planning from %s"
                                 (:declared version-drift)
                                 (:observed version-drift)
                                 current-version))))
        (doseq [{:keys [dep coord path from to]} pin-updates]
          (printf "        pin %-22s %s -> %s  %s\n"
                  dep
                  (ui/c :red from)
                  (if to (ui/c :green to) (ui/c :dim "resolved on release"))
                  (ui/c :dim (format "(%s %s)" (name coord) (fs/file-name path))))))
      (when (= :wait mode)
        (println (ui/c :dim (format "    wait for: %s"
                                    (str/join " " (map (comp str :lib) (:libs (:await w))))))))
      (println))
    (when (seq (:cycles plan))
      (println (ui/c :red "  Cycles blocking part of the cascade:"))
      (doseq [c (:cycles plan)]
        (println (str "    " (ui/c :yellow (str/join " <-> " (sort c)))))))
    (when (seq (:excluded plan))
      (println (ui/c :yellow (format "  %d project(s) excluded:" (count (:excluded plan)))))
      (doseq [{:keys [project reason]} (:excluded plan)]
        (printf "    %-26s %s\n" (ui/c :cyan project) (ui/c :dim (name reason)))))))
