#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0-RC2"]])

(defn valid-password?
  ([password] (valid-password? password false))
  ([password strict?]
   (not-any? #(> % 1)
             (->> (cond->> (clojure.string/split password #"\s")
                    strict? (map (fn [word]
                                   (apply str
                                          (sort (map str word))))))
                  frequencies
                  vals))))

(defn -main [& args]
  (let [m (atom 0)]
    (doseq [line (line-seq (java.io.BufferedReader. *in*))]
      (when (valid-password? line (first args))
        (swap! m inc)))
    (println @m)))
