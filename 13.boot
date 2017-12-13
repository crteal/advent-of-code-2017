#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0"]])

(defn parse-scanner [line]
  (let [[idx layers] (clojure.string/split line #": ")]
    {(Integer/parseInt idx) (Integer/parseInt layers)}))

(def layer-seq
  (memoize
   (fn [layers]
     (cycle
      (concat (range layers)
              (drop-last (reverse (range (dec layers)))))))))

(defn scanner-state [config idx]
  (let [ks (keys config)
        max-k (apply max ks)]
    (reduce
     (fn [m k]
       (if-some [layers (get config k)]
         (assoc m k (nth (layer-seq layers)
                         (mod idx (- (* 2 layers) 2)) idx))
         m))
     {}
     (range (inc max-k)))))

(defn detections
  ([config] (detections config 0 false))
  ([config offset fail-fast?]
   (loop [scanners (range (inc (apply max (keys config))))
          result []]
     (if-some [i (first scanners)]
       (let [state (scanner-state config (+ i offset))]
         (if (and fail-fast? (not (empty? result)))
           result
           (recur
            (rest scanners)
            (if ((fnil zero? -1) (get state i))
              (conj result i)
              result))))
       result))))

(defn score [config result]
  (reduce
   (fn [sum i]
     (+ sum (* i (get config i))))
   0
   result))

(defn delayed [config]
  (loop [i 0]
    (if (empty? (detections config i true))
      i
      (recur (inc i)))))

(defn -main [& args]
  (let [m (atom {})]
    (doseq [line (line-seq (java.io.BufferedReader. *in*))]
      (swap! m merge (parse-scanner line)))
    (println (if (first args)
               (delayed @m)
               (score @m (detections @m))))))
