#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0-RC2"]])

(defn index-of-max [s]
  (loop [i 0
         j 0
         m 0
         coll s]
    (if (empty? coll)
      j
      (recur
       (inc i)
       (if (> (first coll) m)
         i
         j)
       (if (> (first coll) m)
         (first coll)
         m)
       (rest coll)))))

(defn redistribute [s]
  (let [i (index-of-max s)
        v (nth s i)]
    (loop [j (inc i)
           coll (assoc s i 0)]
      (if (> j (+ i v))
        coll
        (recur
         (inc j)
         (update coll (mod j (count coll)) inc))))))

(defn distribution-cycles [s]
  (loop [i 1
         result [(redistribute s)]]
    (if (some (fn [n] (> n 1))
              (vals (frequencies result)))
      i
      (recur
       (inc i)
       (conj result (redistribute (last result)))))))

(defn num-cycles [s]
  (loop [i 1
         result [(redistribute s)]]
    (if (= s (last result))
      i
      (recur
       (inc i)
       (conj result (redistribute (last result)))))))

(defn -main [s & args]
  (let [f (if (first args)
            num-cycles
            distribution-cycles)]
    (println
     (f (reduce
         (fn [memo n]
           (conj memo (Integer/parseInt n)))
         []
         (clojure.string/split s #"\s+"))))))
