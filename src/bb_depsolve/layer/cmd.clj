(ns bb-depsolve.layer.cmd
  "The layers command: check every internal edge against the workspace layer table."
  (:require [babashka.fs :as fs]
            [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.graph.collect :as col]
            [bb-depsolve.layer.check :as check]
            [bb-depsolve.layer.render :as render]
            [bb-depsolve.layer.table :as table]
            [bb-depsolve.release.opts :as ropts]
            [bb-depsolve.layer.freeze :as freeze]))

(defn check-workspace
  "Layer check over the workspace at ROOT-DIR.

   Returns nil when no layer table exists, else the `check/check` result with
   the parsed table under :table and the frozen-layer cadence report under
   :cadence."
  [root-dir {:keys [skip-dirs depth org now-unix window-days]}]
  (when-let [t (table/read-table root-dir)]
    (let [g (col/collect-graph {:root root-dir :skip-dirs skip-dirs
                                :depth depth :org org})]
      (assoc (check/check t (:edges g))
             :table t
             :cadence (freeze/check t (:nodes g) now-unix
                                    (or window-days freeze/default-window-days))))))

(defn report
  "Print RESULT. Returns the count of layer violations plus cadence breaches,
   or 0 when RESULT is nil."
  [root-dir result]
  (if-not result
    (do (render/print-missing-table root-dir) 0)
    (+ (render/print-result (:table result) result)
       (render/print-cadence (:cadence result)))))

(defn layers-cmd
  "Check the workspace dependency graph against the layer table.

   Exits 1 when any edge points up a layer or a frozen layer is over its
   release budget, unless --no-fail is passed."
  [{:keys [opts]}]
  (let [{:keys [root org skip-dirs depth no-fail window-days] :or {root "."}} opts
        root-dir (str (fs/canonicalize root))]
    (ropts/require-org! org "layers")
    (println (ui/c :bold "Checking layer order across the workspace..."))
    (println)
    (let [result (check-workspace root-dir {:skip-dirs (ropts/skip-set skip-dirs)
                                            :depth depth
                                            :org org
                                            :now-unix (quot (System/currentTimeMillis) 1000)
                                            :window-days window-days})
          n (report root-dir result)]
      (when (and (pos? n) (not no-fail))
        (System/exit 1)))))
