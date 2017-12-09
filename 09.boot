#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0-RC2"]])

(defn read-garbage [s]
  (let [eaten (drop-while
               (fn [i]
                 (not (contains? #{"!" ">"} (str i))))
               (rest s))]
    (if (= (str (first eaten)) "!")
      (read-garbage (rest eaten))
      (rest eaten))))

(defn read-groups [s]
  (loop [x (rest s)
         result '(" (list")]
    (if-some [c (first x)]
      (case (str c)
        "," (recur (rest x) result)
        "<" (recur (read-garbage x)
                   result)
        "{" (concat result (read-groups x))
        "}" (recur (rest x) (concat result ")")))
      result)))

(defn take-garbage [s]
  (let [eaten (take-while
               (fn [i]
                 (not (contains? #{"!" ">"} (str i))))
               (rest s))]
    (if (= (str (first (drop (inc (count eaten)) s))) "!")
      (concat eaten (take-garbage (drop (+ (count eaten) 2) s)))
      eaten)))

(defn read-garbage-groups [s]
  (loop [x (rest s)
         result []]
    (if-some [c (first x)]
      (case (str c)
        "," (recur (rest x) result)
        "<" (recur (read-garbage x)
                   (concat result (take-garbage x)))
        "{" (concat result (read-garbage-groups x))
        "}" (recur (rest x) result))
      result)))

(defn score-group [group score]
  (reduce
   (fn [sum child]
     (+ sum (score-group child (inc score))))
   score
   group))

(defn score-groups [s]
  (let [parsed (read-groups s)
        groups (eval (read-string (clojure.string/join "" parsed)))]
    (score-group groups 1)))

(defn garbage-count [s]
  (count (read-garbage-groups s)))

(defn -main [& args]
  (let [f (if (first args)
            garbage-count
            score-groups)]
    (doseq [line (line-seq (java.io.BufferedReader. *in*))]
      (println (f line)))))
