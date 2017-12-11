#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0"]])

;          [0,  1] [1, 1]
; [-1,  0] [0,  0] [1, 0]
; [-1, -1] [0, -1]

(def hex-step-offsets
  {:n  [0 1]
   :ne [1 1]
   :se [1 0]
   :s  [0 -1]
   :sw [-1 -1]
   :nw [-1 0]})

(defn hex-coords
  ([steps] (hex-coords steps [0 0]))
  ([steps origin]
   (reduce
    (fn [[x y] step]
      (let [offset (get hex-step-offsets step)]
        [(+ x (first offset))
         (+ y (last offset))]))
    origin
    steps)))

(defn shortest-path [[x y :as coords]]
  (loop [pos [0 0]
         path []]
    (if (and (= x (first pos))
             (= y (second pos)))
      path
      (let [step (cond
                   (= (second pos) y) (if (neg? x)
                                        :nw
                                        :se)
                   (= (first pos) x) (if (neg? y)
                                       :s
                                       :n)
                   (neg? x) :sw
                   (neg? y) :se
                   :else :ne)]
        (recur
         (hex-coords [step] pos)
         (conj path step))))))

(defn shortest-paths [steps]
  (get
   (reduce
    (fn [{:keys [coord paths]} step]
      (let [next-coord (hex-coords [step] coord)]
        {:coord next-coord :paths (conj paths (shortest-path next-coord))}))
    {:coord [0 0] :paths []}
    steps)
   :paths))

(defn longest-path [steps]
  (reduce
   (fn [m path]
     (if (> (count path) (count m))
       path
       m))
   []
   (shortest-paths steps)))

(defn -main [& args]
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (let [steps (map keyword (clojure.string/split line #","))]
      (println (count
                (if (first args)
                  (longest-path steps)
                  (shortest-path (hex-coords steps))))))))
