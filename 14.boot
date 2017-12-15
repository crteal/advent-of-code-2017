#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0"]])

(defn hash-to-binary [h]
  (reduce
   (fn [b c]
     (str b (clojure.pprint/cl-format nil
                                      "~4,'0',B"
                                      (Integer/parseInt (str c) 16) 2)))
   ""
   h))

(defn occupied-squares [grid]
  (reduce
   (fn [sum row]
     (reduce #(if (= %2 \1) (inc %1) %1) sum row))
   0
   grid))

(defn adjacents [[x y] grid]
  (reduce
   (fn [m location]
     (let [candidate-x (+ x (first location))
           candidate-y (+ y (last location))]
       (if (or (>= candidate-x (count grid))
               (neg? candidate-x)
               (>= candidate-y (count grid))
               (neg? candidate-y)
               (not= (nth (nth grid candidate-y) candidate-x) \1))
         m
         (conj m [candidate-x candidate-y]))))
   []
   [[0 -1] [1 0] [0 1] [-1 0]]))

(defn visit-region [origin grid]
  (reduce
   (fn reducer [region point]
     (let [adj (filter #(not (contains? region %))
                       (adjacents point grid))]
       (reduce reducer (conj region point) adj)))
   #{}
   [origin]))

(defn build-regions [grid]
  (reduce
   (fn [m j]
     (reduce
      (fn [{:keys [regions visited]} i]
        (let [region (when-not (or (contains? visited [i j])
                                   (= (nth (nth grid j) i) \0))
                       (visit-region [i j] grid))]
          {:regions (if (empty? region)
                      regions
                      (conj regions region))
           :visited (clojure.set/union visited #{[i j]} region)}))
      m
      (range (count grid))))
   {:regions [] :visited #{}}
   (range (count grid))))

(defn -main [& args]
  (let [m (atom [])]
    (doseq [line (line-seq (java.io.BufferedReader. *in*))]
      (swap! m conj (hash-to-binary line)))
    (println (if (first args)
               (count (get (build-regions @m) :regions))
               (occupied-squares @m)))))
