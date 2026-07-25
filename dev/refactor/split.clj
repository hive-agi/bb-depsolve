(ns refactor.split
  "Namespace-splitting tooling: slice one namespace into single-responsibility
   submodules behind a re-exporting facade.

   A split is described as data — an OWNER map of symbol -> module keyword plus
   the surrounding decisions — and applied by `split!`. Symbol requalification
   runs over the parse tree, so occurrences inside strings and docstrings are
   never rewritten.

   A module is a keyword (a submodule of :base-ns) or a string (an absolute
   namespace, for vars that move out of the tree entirely).

   Drive from the REPL; see the rich comment at the foot of this file."
  (:require [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [rewrite-clj.zip :as z]
            [clojure.java.io :as io]))

;; =============================================================================
;; Parsing
;; =============================================================================

(defn top-level-forms
  "Top-level forms of the file at PATH as [{:text :sexpr}], comments and
   whitespace removed."
  [path]
  (->> (:children (p/parse-string-all (slurp path)))
       (remove n/whitespace-or-comment?)
       (mapv (fn [node] {:text (n/string node) :sexpr (n/sexpr node)}))))

(def definition-heads
  "Top-level heads whose second element names what the form defines."
  '#{defn defn- def deftest defspec defmacro defmulti defrecord defprotocol})

(defn form-name
  "Symbol a definition form defines, else nil."
  [sexpr]
  (let [[head target] sexpr]
    (when (and (contains? definition-heads head) (symbol? target))
      target)))

(defn private?
  [{:keys [text sexpr]}]
  (or (= 'defn- (first sexpr))
      (str/starts-with? text "(def ^:private ")))

;; =============================================================================
;; Module naming
;; =============================================================================

(defn module-ns
  "Full namespace name of MODULE."
  [base-ns module]
  (if (string? module) module (str base-ns "." (name module))))

(defn module-alias
  "Alias MODULE is required under."
  [module]
  (if (string? module)
    (last (str/split module #"\."))
    (name module)))

(defn module-file
  [source-dir module]
  (str source-dir "/" (str/replace (name module) "-" "_") ".clj"))

;; =============================================================================
;; Rewriting
;; =============================================================================

(defn qualify
  "TEXT with every symbol token named in RENAME replaced by its mapping.
   Tree-level, so string and docstring contents are left alone."
  [text rename]
  (if (empty? rename)
    text
    (-> (z/of-string text {:track-position? false})
        (z/prewalk (fn [zloc]
                     (and (= :token (z/tag zloc))
                          (symbol? (z/sexpr zloc))
                          (contains? rename (z/sexpr zloc))))
                   (fn [zloc] (z/replace zloc (get rename (z/sexpr zloc)))))
        z/root-string)))

(defn make-public
  "TEXT with NM's definition promoted from private to public."
  [text nm]
  (-> text
      (str/replace (str "(defn- " nm) (str "(defn " nm))
      (str/replace (str "(def ^:private " nm) (str "(def " nm))))

(defn rename-map
  "Symbol -> qualified symbol for everything MODULE must reach through an alias:
   every var owned by another module, plus the shared vars in :external."
  [{:keys [owner external]} module]
  (into (into {} (for [[sym alias] external] [sym (symbol (str alias "/" sym))]))
        (for [[sym m] owner :when (not= m module)]
          [sym (symbol (str (module-alias m) "/" sym))])))

;; =============================================================================
;; Emission
;; =============================================================================

(defn requires-for
  "Require entries BODY actually uses: always-requires, then base requires whose
   alias appears, then one per sibling module referenced."
  [{:keys [base-ns base-requires always-requires]} body siblings]
  (concat always-requires
          (->> base-requires
               (filter (fn [[alias _]] (str/includes? body (str alias "/"))))
               (map second))
          (->> siblings
               (filter #(str/includes? body (str (module-alias %) "/")))
               (sort-by module-alias)
               (map #(str "[" (module-ns base-ns %) " :as " (module-alias %) "]")))))

(defn module-source
  [{:keys [base-ns module-doc promote-public] :as spec} module forms siblings]
  (let [rename (rename-map spec module)
        body (->> forms
                  (map (fn [{:keys [text sexpr]}]
                         (let [nm (form-name sexpr)
                               t (qualify text rename)]
                           (cond-> t
                             (contains? promote-public nm) (make-public nm)))))
                  (str/join "\n\n"))
        reqs (requires-for spec body siblings)]
    (str "(ns " (module-ns base-ns module) "\n"
         "  \"" (get module-doc module) "\""
         (when (seq reqs)
           (str "\n  (:require " (str/join "\n            " reqs) ")"))
         ")\n\n"
         body "\n")))

(defn facade-source
  "The original namespace reduced to re-exports. Private vars are omitted —
   a private var cannot be aliased across a namespace boundary."
  [{:keys [base-ns facade-doc extra-exports]} exports]
  (let [exports (concat exports extra-exports)
        modules (->> (map second exports) distinct (sort-by module-alias))]
    (str "(ns " base-ns "\n"
         "  \"" facade-doc "\"\n"
         "  (:require "
         (str/join "\n            "
                   (map #(str "[" (module-ns base-ns %) " :as " (module-alias %) "]")
                        modules))
         "))\n\n"
         (->> exports
              (map (fn [[nm m]] (str "(def " nm " " (module-alias m) "/" nm ")")))
              (str/join "\n"))
         "\n")))

;; =============================================================================
;; Driver
;; =============================================================================

(defn classify
  [{:keys [source-file owner drop-forms]}]
  (keep (fn [{:keys [sexpr] :as form}]
          (let [nm (form-name sexpr)]
            (when-not (or (contains? '#{ns declare} (first sexpr))
                          (contains? (set drop-forms) nm))
              (assoc form
                     :name nm
                     :private? (private? form)
                     :owner (or (get owner nm)
                                (throw (ex-info "unclassified form" {:name nm})))))))
        (top-level-forms source-file)))

(defn split!
  "Slice SPEC's :source-file into one file per module.

   What becomes of the original depends on the spec:
     :keep-module m  - m's forms are written back to :source-file. m must be a
                       STRING naming the original namespace, so it is treated
                       as an absolute module and keeps its own name.
     :facade? true   - (default) the original becomes a re-exporting facade.
     :facade? false  - the original is deleted, which is what a test namespace
                       wants: re-exporting tests is meaningless.

   Refuses a spec whose :source-file is gone: a split spec is spent once
   applied, and a moved or already-split source means the spec describes a
   file that no longer exists.

   Returns a path -> form-count map."
  [{:keys [source-file source-dir facade? keep-module] :or {facade? true} :as spec}]
  (when-not (.exists (io/file source-file))
    (throw (ex-info (str "split spec points at a source file that does not exist: "
                         source-file
                         " — the spec is stale (already applied, or the namespace moved)")
                    {:source-file source-file :source-dir source-dir})))
  (let [classified (classify spec)
        grouped (group-by :owner classified)
        modules (keys grouped)
        siblings (fn [m] (remove #{m} modules))]
    (into {}
          (concat
           (for [[module forms] (dissoc grouped keep-module)]
             (let [path (module-file source-dir module)]
               (spit path (module-source spec module forms (siblings module)))
               [path (count forms)]))
           (cond
             keep-module
             [(let [forms (get grouped keep-module)]
                (spit source-file
                      (module-source spec keep-module forms (siblings keep-module)))
                [source-file (count forms)])]

             facade?
             [(let [exports (->> classified (remove :private?) (map (juxt :name :owner)))]
                (spit source-file (facade-source spec exports))
                [source-file (count exports)])]

             :else
             (do (io/delete-file source-file true)
                 [[(str source-file " (deleted)") 0]]))))))

(defn repoint!
  "Rewrite each file in FILES so references to the facade resolve against the
   owning submodule instead. The facade snapshots values, so a caller that
   reaches through it cannot be redefined at a test seam.

   Symbol rewriting only; requires are then reconciled with carto.
   Returns file -> {:renamed n :needs #{module}}."
  [{:keys [owner external]} files]
  (into {}
        (for [path files]
          (let [src (slurp path)
                alias (second (re-find #"\[bb-depsolve\.core :as ([\w.-]+)\]" src))]
            (if-not alias
              [path {:renamed 0 :needs #{}}]
              (let [rename (into {}
                                 (concat
                                  (for [[sym m] owner]
                                    [(symbol alias (str sym))
                                     (symbol (module-alias m) (str sym))])
                                  (for [[sym a] external]
                                    [(symbol alias (str sym)) (symbol a (str sym))])))
                    forms (top-level-forms path)
                    needs (atom #{})
                    body (->> forms
                              (map (fn [{:keys [text sexpr]}]
                                     (if (= 'ns (first sexpr))
                                       text
                                       (let [out (qualify text rename)]
                                         (doseq [[sym m] owner
                                                 :when (str/includes?
                                                        out (str (module-alias m) "/" sym))]
                                           (swap! needs conj m))
                                         (doseq [[sym a] external
                                                 :when (str/includes? out (str a "/" sym))]
                                           (swap! needs conj a))
                                         out))))
                              (str/join "\n\n"))
                    renamed (- (count (re-seq (re-pattern (str "\\b" alias "/")) src))
                               (count (re-seq (re-pattern (str "\\b" alias "/")) body)))]
                (spit path (str body "\n"))
                [path {:renamed renamed :needs @needs}]))))))

(comment
  (require '[refactor.core-split :as cs] :reload)
  (split! cs/spec)
  ,)