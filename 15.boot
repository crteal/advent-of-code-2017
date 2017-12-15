#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0-RC2"]])

(defn make-generator
  ([i factor] (make-generator i factor 1))
  ([i factor multiple]
   (iterate (fn gen [n]
              (let [m (mod (* n factor) 2147483647)]
                (if (zero? (mod m multiple))
                  m
                  (gen m))))
            i)))

(defn adjudicate [a-seed a-multiple b-seed b-multiple pairs]
  (reduce
   (fn [sum [a b]]
     (if (= (take-last 16 (Integer/toBinaryString a))
            (take-last 16 (Integer/toBinaryString b)))
       (inc sum)
       sum))
   0
   (take pairs
         (partition 2
                    (interleave (make-generator a-seed 16807 a-multiple)
                                (make-generator b-seed 48271 b-multiple))))))

(defn part-1 [a-seed b-seed pairs]
  (adjudicate a-seed 1 b-seed 1 pairs))

(defn part-2 [a-seed b-seed pairs]
  (adjudicate a-seed 4 b-seed 8 pairs))

(defn -main [a-seed b-seed pairs & args]
  (let [f (if (first args)
            part-2
            part-1)]
    (println (f (Integer/parseInt a-seed)
                (Integer/parseInt b-seed)
                (Integer/parseInt pairs)))))
