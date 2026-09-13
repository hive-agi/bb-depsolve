(ns bb-depsolve.core.localroot
  "Structural :local/root scan over a dep file.

   `version.parse/find-local-deps` is a regex over the file text: it sees every
   occurrence but knows neither WHERE the occurrence sits (top-level :deps vs an
   opt-in dev alias) nor WHETHER it is a violation at all. Both distinctions
   decide what lint may say and what --fix may do:

   - A :local/root under an alias is an opt-in dev override. Rewriting it to a
     published coordinate changes what `-M:that-alias` resolves to; the repair is
     to MOVE the alias into the untracked local.deps.edn, not to re-pin it.
   - A :local/root whose path stays inside the project's own repo (clojure-lsp's
     lib/cli, mcp-clojure-sdk's utils/logger) resolves on every clone. It is a
     subproject reference, not a machine-specific path.
   - A dep file that is in no git repo at all ships to nobody.

   Edits are cut-and-paste over the exact source span rewrite-clj reports, so
   every byte outside the moved alias (comments, alignment) survives untouched."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [rewrite-clj.zip :as z]))

(def dep-maps
  "Keys whose value is a coordinate map, inside an alias."
  #{:deps :extra-deps :override-deps :replace-deps})

(defn- entries
  "Seq of [key-zloc value-zloc] for a zipper positioned at a map node."
  [zloc]
  (when (and zloc (= :map (z/tag zloc)))
    (loop [k (z/down zloc) acc []]
      (if (nil? k)
        acc
        (let [v (z/right k)]
          (recur (some-> v z/right) (cond-> acc v (conj [k v]))))))))

(defn- val-at [map-zloc kw]
  (some (fn [[k v]] (when (= kw (try (z/sexpr k) (catch Exception _ nil))) v))
        (entries map-zloc)))

(defn- local-root-of [v-zloc]
  (let [sx (try (z/sexpr v-zloc) (catch Exception _ nil))]
    (when (map? sx) (get sx :local/root))))

(defn- coord-entries [map-zloc scope]
  (for [[k v] (entries map-zloc)
        :let [path (local-root-of v)]
        :when path]
    {:lib (z/sexpr k) :path path :scope scope}))

(defn- parse [content]
  (let [z (try (z/of-string content {:track-position? true}) (catch Exception _ nil))]
    (when (and z (= :map (z/tag z))) z)))

(defn scan
  "Every :local/root in dep-file CONTENT, with its scope.

   Scope is {:kind :deps} for a top-level dep, or
   {:kind :alias :alias <kw> :key <:extra-deps|:override-deps|...>}.
   Returns [] for content that does not parse as a map."
  [content]
  (if-let [root (parse content)]
    (let [top   (coord-entries (val-at root :deps) {:kind :deps})
          alias (for [[ak av] (entries (val-at root :aliases))
                      [ik iv] (entries av)
                      :when (contains? dep-maps (z/sexpr ik))
                      hit (coord-entries iv {:kind  :alias
                                             :alias (z/sexpr ak)
                                             :key   (z/sexpr ik)})]
                  hit)]
      (vec (concat top alias)))
    []))

(defn- git-root
  "Nearest ancestor directory of `dir` holding .git, or nil."
  [dir]
  (loop [d (fs/canonicalize dir)]
    (cond
      (nil? d)                        nil
      (fs/exists? (fs/path d ".git")) (str d)
      :else                           (recur (fs/parent d)))))

(defn classify
  "Why (or whether) one :local/root hit is a violation of the mvn-only rule.

   :no-repo   the dep file is in no git repo, so nothing about it is committed
   :in-repo   the path resolves inside the project's own repo: portable on clone
   :violation otherwise: a path only this machine has."
  [{:keys [path]} dep-file]
  (let [dir  (fs/parent (fs/canonicalize dep-file))
        root (git-root dir)]
    (if (nil? root)
      :no-repo
      (let [target (str (fs/normalize (fs/path dir path)))]
        (if (or (= target root) (str/starts-with? target (str root "/")))
          :in-repo
          :violation)))))

;; ---------------------------------------------------------------- source spans

(defn- offset
  "Character offset of 1-based [row col] in `content`."
  [content row col]
  (loop [i 0 r 1]
    (if (= r row)
      (+ i (dec col))
      (recur (inc (str/index-of content "\n" i)) (inc r)))))

(defn- span
  "[start end) character span of the node at `zloc` in `content`."
  [content zloc]
  (let [[row col] (z/position zloc)
        start     (offset content row col)]
    [start (+ start (count (z/string zloc)))]))

(defn- own-line?
  "True when the comment at `zloc` starts its own line, rather than trailing the
   code before it. A trailing comment documents the entry it follows, so it must
   NOT travel with the next one."
  [zloc]
  (loop [z (z/left* zloc)]
    (cond
      (nil? z)                          true
      (= :newline (z/tag z))            true
      (= :comment (z/tag z))            true
      (= :whitespace (z/tag z))         (recur (z/left* z))
      :else                             false)))

(defn- comment-zlocs
  "Contiguous own-line comment nodes directly above `zloc`, outermost first.
   rewrite-clj gives each its own position, so a comment sharing a line with the
   opening brace (`{;; like this`) is found where a line scan would miss it."
  [zloc]
  (loop [z (z/left* zloc) acc []]
    (cond
      (nil? z)                  acc
      (= :comment (z/tag z))    (if (own-line? z)
                                  (recur (z/left* z) (cons z acc))
                                  acc)
      (#{:whitespace :newline} (z/tag z)) (recur (z/left* z) acc)
      :else                     acc)))

(defn extract-alias
  "Cut alias `alias-kw` out of the :aliases map in `content`.

   Returns {:content <content without it> :source \"<kw> <value>\"}, the source
   carrying the entry's own comment lines, or nil when the alias is absent."
  [content alias-kw]
  (when-let [root (parse content)]
    (when-let [aliases (val-at root :aliases)]
      (when-let [k-zloc (some (fn [[k _]] (when (= alias-kw (z/sexpr k)) k))
                              (entries aliases))]
        (let [v-zloc      (z/right k-zloc)
              [k-start _] (span content k-zloc)
              [_ v-end]   (span content v-zloc)
              cmts        (comment-zlocs k-zloc)
              c-start     (if (seq cmts)
                            (first (span content (first cmts)))
                            k-start)
              src         (str/trim (subs content c-start v-end))
              ;; take the rest of the entry's last line and one blank line with it
              tail-end    (let [nl (str/index-of content "\n" v-end)]
                            (if nl (inc nl) (count content)))
              cut         (str (subs content 0 c-start) (subs content tail-end))]
          {:content (str/replace cut #"\n\n\n+" "\n\n")
           :source  src})))))

(defn splice-alias
  "Add `alias-src` under :aliases in local.deps.edn `content` (nil when the file
   does not exist yet). Keeps whatever the file already holds, comments and all."
  [content alias-src]
  (let [root (some-> content parse)]
    (cond
      (nil? root)
      (str "{:aliases\n {" alias-src "}}\n")

      (val-at root :aliases)
      (let [[_ a-end] (span content (val-at root :aliases))
            brace     (dec a-end)]
        (str (str/trimr (subs content 0 brace)) "\n\n  " alias-src "}"
             (subs content a-end)))

      :else
      (let [[_ r-end] (span content root)
            brace     (dec r-end)]
        (str (str/trimr (subs content 0 brace)) "\n\n :aliases\n {" alias-src "}}"
             (subs content r-end))))))
