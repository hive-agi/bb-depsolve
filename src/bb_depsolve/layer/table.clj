(ns bb-depsolve.layer.table
  "Read and index the workspace layer table.

   File format, at the workspace root:

     {:layers  [{:name :contracts :projects [\"hive-spi\"]}
                {:name :data      :projects [\"hive-dsl\"]}]
      :waivers [{:from \"a\" :to \"b\" :reason \"...\"}]}

   :layers is ORDERED — a layer's index is its level, lowest first."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]))

(def default-file-name "depsolve-layers.edn")

(defn index-levels
  "{project -> level-index} over an ordered :layers vector."
  [layers]
  (into {}
        (for [[i {:keys [projects]}] (map-indexed vector layers)
              p projects]
          [p i])))

(defn layer-names
  "The :name of each layer, in level order."
  [layers]
  (mapv :name layers))

(defn parse
  "Raw table edn -> {:levels {} :names [] :terminal #{} :waivers #{} :reasons {}}.

   :terminal holds the level indices marked :terminal? — nothing may depend on
   a project in one. :waivers holds [from to] pairs; :reasons maps that pair
   to its :reason."
  [{:keys [layers waivers]}]
  {:levels   (index-levels layers)
   :names    (layer-names layers)
   :terminal (into #{} (keep-indexed (fn [i l] (when (:terminal? l) i))) layers)
   :waivers  (into #{} (map (juxt :from :to)) waivers)
   :reasons  (into {} (map (juxt (juxt :from :to) :reason)) waivers)})

(defn table-path
  "Where the layer table lives under ROOT."
  [root]
  (str (fs/path root default-file-name)))

(defn read-table
  "The parsed table under ROOT, or nil when no table file exists."
  [root]
  (let [p (table-path root)]
    (when (fs/exists? p)
      (parse (edn/read-string (slurp p))))))

(defn level-of
  "PROJECT's level index, or nil when the project is unranked."
  [table project]
  (get-in table [:levels project]))

(defn level-name
  "The layer name at LEVEL, or nil."
  [table level]
  (when level (get (:names table) level)))

(defn unranked
  "Projects in PROJECTS that the table does not rank, sorted."
  [table projects]
  (vec (sort (remove #(level-of table %) projects))))
