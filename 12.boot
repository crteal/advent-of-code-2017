#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0"]])

(defn program-connections
  ([graph program] (program-connections graph program #{program}))
  ([graph program seen]
   (reduce
    (fn [m next-program]
      (if (contains? m next-program)
        m
        (program-connections graph next-program (conj m next-program))))
    seen
    (get graph program))))

(defn program-groups [graph]
  (reduce
   (fn [m program]
     (let [seen (apply clojure.set/union m)]
       (if (contains? seen program)
         m
         (conj m (program-connections graph program)))))
   []
   (keys graph)))

(defn parse-program [line]
  (let [[program connections] (clojure.string/split line #" <-> ")]
    {(Integer/parseInt program)
     (set (map
           #(Integer/parseInt %)
           (clojure.string/split connections #", ")))}))

(defn -main [& args]
  (let [m (atom {})]
    (doseq [line (line-seq (java.io.BufferedReader. *in*))]
      (swap! m merge (parse-program line)))
    (println (count (if (first args)
                      (program-groups @m)
                      (program-connections @m 0))))))
