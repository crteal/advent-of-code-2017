#!/usr/bin/env boot

(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0-RC2"]])

(def registers (atom [{}]))

(defmacro execute [instruction]
  (let [{:keys [cond-op op reg cond-reg] :as parts}
        (apply hash-map (interleave
                         [:reg :op :val :cond :cond-reg :cond-op :cond-val]
                         (clojure.string/split instruction #"\s+")))
        op (symbol (cond
                     (= op "inc") '+
                     (= op "dec") '-))
        cond-op (symbol (cond
                          (= cond-op "==") "="
                          (= cond-op "!=") "not="
                          :else cond-op))]
    `(let [~(symbol reg) (get (last @registers) ~reg 0)
           ~(symbol cond-reg) (get (last @registers) ~cond-reg 0)]
       (~(symbol (:cond parts)) (~cond-op ~(symbol cond-reg) ~(Integer/parseInt (:cond-val parts)))
                                (swap! registers conj (assoc (last @registers) ~reg (~op ~(symbol reg) ~(Integer/parseInt (:val parts)))))))))

(defn max-value [stack]
  (reduce
   (fn [{:keys [mx] :as m} k]
     (let [v (get stack k)]
       (if (>= v mx)
         {:mx v :key k}
         m)))
   {:mx Integer/MIN_VALUE}
   (keys stack)))

(defn max-stack-value [states]
  (reduce
   (fn [{:keys [mx] :as m} stack]
     (let [mv (max-value stack)]
       (if (>= (:mx mv) mx) mv m)))
   (max-value (first states))
   (rest states)))

;; TODO figure out why reading from file did not work with macro
(defn -main [& args]
  (let [f (if (first args)
            max-stack-value
            #(max-value (last %)))]
    (println (f @registers))))
