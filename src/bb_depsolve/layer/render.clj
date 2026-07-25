(ns bb-depsolve.layer.render
  "Console rendering for a layer check result."
  (:require [bb-depsolve.cli.ui :as ui]
            [bb-depsolve.layer.table :as table]
            [clojure.string :as str]))

(defn violation-line
  "One rendered violation row."
  [{:keys [from to from-layer to-layer to-terminal?]}]
  (format "  %-26s -> %-24s %s"
          (ui/c :cyan from)
          (ui/c :red to)
          (ui/c :dim (if to-terminal?
                       (format "%s is terminal — nothing may depend on it"
                               (name (or to-layer :?)))
                       (format "%s depends UP on %s"
                               (name (or from-layer :?))
                               (name (or to-layer :?)))))))

(defn print-table
  "Print the layer order the check ran against."
  [{:keys [names levels terminal]}]
  (println (ui/c :bold "Layer order (lowest first):"))
  (doseq [[i nm] (map-indexed vector names)
          :let [members (sort (keep (fn [[p l]] (when (= l i) p)) levels))]]
    (printf "  L%d %-16s %s\n"
            i
            (str (name nm) (when (contains? terminal i) " *"))
            (ui/c :dim (str/join " " members))))
  (when (seq terminal)
    (println (ui/c :dim "  * terminal — nothing may depend on it")))
  (println))

(defn print-unranked
  "Warn about graph projects the table does not rank."
  [unranked]
  (when (seq unranked)
    (println (ui/c :yellow (format "%d project(s) not ranked by the table (exempt):"
                                   (count unranked))))
    (println (ui/c :dim (str "  " (str/join " " unranked))))
    (println)))

(defn print-result
  "Print a full layer check result. Returns the violation count."
  [table {:keys [violations summary unranked]}]
  (print-table table)
  (print-unranked unranked)
  (if (empty? violations)
    (println (ui/c :green "No layer violations. Every edge points down or sideways."))
    (do
      (println (ui/c :red (format "%d layer violation(s): a project pins something ABOVE it."
                                  (count violations))))
      (println)
      (doseq [v violations] (println (violation-line v)))))
  (println)
  (println (ui/c :dim (str "  " (pr-str summary))))
  (println)
  (count violations))

(defn print-missing-table
  "Explain that no layer table was found under ROOT."
  [root]
  (println (ui/c :yellow (str "No " table/default-file-name " at " root)))
  (println (ui/c :dim "  Layer checking is skipped. Create the file to enable it:"))
  (println (ui/c :dim "    {:layers [{:name :contracts :projects [\"lib-a\"]}"))
  (println (ui/c :dim "              {:name :apps      :projects [\"app-b\"]}]}"))
  (println))
