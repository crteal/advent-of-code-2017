#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0"]])

(defn selection [idx n coll]
  (let [{:keys [indices values]} (reduce
                                  (fn [m offset]
                                    (let [i (mod (+ idx offset) (count coll))]
                                      (merge-with conj m {:indices i
                                                          :values (nth coll i)})))
                                  {:indices [] :values []}
                                  (range n))]
    (reduce
     (fn [m [idx val]]
       (assoc m idx val))
     coll
     (partition 2 (interleave indices (reverse values))))))

(defn round
  [{:keys [idx skip coll] :or {idx 0 skip 0}} lengths]
  (loop [i idx
         s skip
         c coll
         l lengths]
    (if-some [length (first l)]
      (recur
       (+ i length s)
       (inc s)
       (selection i length c)
       (rest l))
      {:idx i :skip s :coll c})))

(defn read-integers [line]
  (map
   #(Integer/parseInt %)
   (clojure.string/split line #",")))

(defn read-ascii [line]
  (reduce
   (fn [m c]
     (conj m (int c)))
   '(17 31 73 47 23)
   (reverse line)))

(defn tie-knot [line]
  (let [lengths (read-integers line)]
    (round {:coll (vec (range 256))} lengths)))

(defn verify-knot [line]
  (apply *
         (take 2 (get (tie-knot line) :coll))))

(defn sparse-hash [line]
  (let [lengths (read-ascii line)]
    (reduce
     (fn [m i]
       (merge m (round m lengths)))
     {:coll (vec (range 256))}
     (range 64))))

(defn dense-hash [coll]
  (map
   (fn [block]
     (apply bit-xor block))
   (partition 16 coll)))

(defn knot-hash [line]
  (map
   #(format "%02x" %)
   (dense-hash
    (get (sparse-hash line) :coll))))

(defn -main [& args]
  (let [f (if (first args)
            knot-hash
            verify-knot)]
    (doseq [line (line-seq (java.io.BufferedReader. *in*))]
      (println (f line)))))
