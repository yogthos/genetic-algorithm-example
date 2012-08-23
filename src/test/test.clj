(ns test.test
   (:require (ga [main :as ga])) 
  (:gen-class )) 
  
;example usage
(defn -main [string]
  (let [target  (vec string)
        mutator (fn[_] (char (rand-in-range 32 126))) 
        fitness (fn [target value]
                  (reduce 
                    (fn [rank [a b]] (if (= a b) rank (dec rank)))
                    0 (map vector value target)))]  
    (time (println (apply str (:value (first (evolve 500 0.01 target mutator fitness))))))))

(-main "Hello World!")