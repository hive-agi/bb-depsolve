(ns bb-depsolve.release.await
  "Polling a wave's artifacts until they publish.

   entry {:lib :newer-than :expect :state}
   state :pending | :resolved | :unreachable"
  (:require [bb-depsolve.release.port :as port]
            [bb-depsolve.cli.ui :as ui]
            [hive-dsl.result :as r]
            [clojure.string :as str]))

(def default-backoff
  {:start-ms 2000 :cap-ms 15000})

(defn backoff-delays
  "Infinite seq of poll delays: :start-ms doubling, capped at :cap-ms."
  [{:keys [start-ms cap-ms] :or {start-ms 2000 cap-ms 15000}}]
  (iterate #(min (long cap-ms) (* 2 (long %))) (long start-ms)))

(defn resolved?
  [entry]
  (= :resolved (:state entry)))

(defn- probe
  [registry entry]
  (let [result (port/await-satisfied? registry entry)]
    (assoc entry :state (cond
                          (not (r/ok? result)) :unreachable
                          (:ok result) :resolved
                          :else :pending))))

(defn- wanted
  [{:keys [expect newer-than]}]
  (cond
    expect (str "= " expect)
    newer-than (str "> " newer-than)
    :else "any release"))

(defn render-lines
  "Display lines for ENTRIES at ELAPSED-MS."
  [entries elapsed-ms]
  (let [pending (remove resolved? entries)
        width (reduce max 0 (map #(count (str (:lib %))) entries))
        secs (quot (long elapsed-ms) 1000)]
    (into [(if (seq pending)
             (format "waiting on %d of %d artifact(s) — %ds elapsed"
                     (count pending) (count entries) secs)
             (format "all %d artifact(s) published — %ds elapsed"
                     (count entries) secs))]
          (for [entry (sort-by (comp str :lib) entries)]
            (str "  "
                 (case (:state entry)
                   :resolved (ui/c :green "✔")
                   :unreachable (ui/c :red "✖")
                   (ui/c :yellow "…"))
                 " "
                 (ui/pad-right (str (:lib entry)) width)
                 "  "
                 (case (:state entry)
                   :resolved (ui/c :dim "published")
                   :unreachable (ui/c :red "registry unreachable")
                   (ui/c :dim (str "waiting " (wanted entry)))))))))

(defn format-timeout
  "Failure text for an :await/timeout error value."
  [{:keys [unresolved elapsed-ms timeout-ms]}]
  (str (ui/c :red (format "await timed out after %ds (limit %ds)"
                          (quot (long elapsed-ms) 1000)
                          (quot (long timeout-ms) 1000)))
       "\n  never published: "
       (str/join ", " (map str unresolved))
       "\n  re-run with --no-wait to plan past it."))

(defn tty-emitter
  "Emitter redrawing its frame in place. => (fn [entries elapsed-ms])."
  []
  (let [drawn (atom 0)]
    (fn [entries elapsed-ms]
      (let [lines (render-lines entries elapsed-ms)]
        (when (pos? (long @drawn))
          (print (str "\033[" @drawn "A")))
        (doseq [line lines]
          (println (str "\033[2K" line)))
        (flush)
        (reset! drawn (count lines))))))

(defn plain-emitter
  "Emitter printing one line per state change. => (fn [entries elapsed-ms])."
  []
  (let [seen (atom {})]
    (fn [entries elapsed-ms]
      (let [secs (quot (long elapsed-ms) 1000)]
        (when (empty? @seen)
          (println (format "waiting on %d artifact(s)" (count entries))))
        (doseq [{:keys [lib state]} (sort-by (comp str :lib) entries)
                :when (not= state (get @seen lib))]
          (println (format "  %-12s %s (%ds)" (name state) (str lib) secs))
          (swap! seen assoc lib state))))))

(defn default-emitter
  []
  (if (ui/tty?) (tty-emitter) (plain-emitter)))

(defn await-wave!
  "Poll REGISTRY until every lib in DIRECTIVE resolves.

   DIRECTIVE is a wave's :await — {:mode :timeout-ms :libs}. :mode :skip or an
   empty :libs is a no-op.

   OPTS seams: :emit (fn [entries elapsed-ms]), :sleep (fn [ms]),
   :now (fn => ms), :backoff {:start-ms :cap-ms}.

   => Result of {:mode :elapsed-ms :libs [entry]}, or an error
   {:kind :await/timeout :unresolved :elapsed-ms :timeout-ms}."
  ([registry directive] (await-wave! registry directive {}))
  ([registry {:keys [mode timeout-ms libs]} opts]
   (let [now (get opts :now #(System/currentTimeMillis))
         sleep (get opts :sleep #(Thread/sleep (long %)))
         emit (get opts :emit (default-emitter))
         t0 (long (now))]
     (if (or (= :skip mode) (empty? libs))
       (r/ok {:mode (or mode :skip) :elapsed-ms 0 :libs []})
       (loop [delays (backoff-delays (get opts :backoff default-backoff))
              entries (mapv #(assoc % :state :pending) libs)]
         (let [probed (mapv #(probe registry %) entries)
               elapsed (- (long (now)) t0)]
           (emit probed elapsed)
           (cond
             (every? resolved? probed)
             (r/ok {:mode :wait :elapsed-ms elapsed :libs probed})

             (>= elapsed (long (or timeout-ms 0)))
             (r/err {:kind :await/timeout
                     :unresolved (mapv :lib (remove resolved? probed))
                     :elapsed-ms elapsed
                     :timeout-ms (long (or timeout-ms 0))})

             :else
             (do (sleep (first delays))
                 (recur (rest delays) probed)))))))))
