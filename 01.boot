#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0-RC2"]])

(defn sum-matching-interval [s interval]
  (let [l (count s)]
    (reduce
     +
     (map-indexed
      (fn [idx n]
        (if (= n (nth s (mod (+ idx interval) l))) n 0))
      s))))

(defn -main [s & args]
  (println
   (sum-matching-interval
    (map #(Character/getNumericValue %) s)
    (if-some [interval (first args)]
      (Integer/parseInt interval)
      1))))
