#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0-RC2"]])

(defn jumps [coll strange?]
  (loop [i 0
         idx 0
         items (into [] coll)]
    (if (or (>= idx (count items)) (< idx 0))
      i
      (recur
       (inc i)
       (+ idx (nth items idx))
       (update items idx (if (and strange? (>= (nth items idx) 3)) dec inc))))))

(defn -main [& args]
  (let [m (atom [])]
    (doseq [line (line-seq (java.io.BufferedReader. *in*))]
      (swap! m conj (Integer/parseInt line)))
    (println (jumps @m (first args)))))
