#!/usr/bin/env boot

(set-env!
  :dependencies '[[org.clojure/clojure "1.9.0-RC2"]])

(defn line-checksum [line]
  (let [numbers (map #(Integer/parseInt %)
                     (clojure.string/split line #"\s+"))]
    (- (apply max numbers) (apply min numbers))))

(defn line-checksum-evd [line]
  (loop [numbers (map #(Integer/parseInt %)
                      (clojure.string/split line #"\s+"))]
    (if-some [evd (reduce
                    (fn [m b]
                      (let [a (first numbers)]
                        (cond
                          (zero? (mod a b)) (reduced (/ a b))
                          (zero? (mod b a)) (reduced (/ b a))
                          :else m)))
                    nil
                    (rest numbers))]
      evd
      (recur (rest numbers)))))

(defn -main [& args]
  (let [m (atom [])
        f (if (= (first args) "divisible")
            line-checksum-evd
            line-checksum)]
    (doseq [line (line-seq (java.io.BufferedReader. *in*))]
      (swap! m conj (f line)))
    (println (apply + @m))))
