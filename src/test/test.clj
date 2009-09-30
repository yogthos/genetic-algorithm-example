(ns test.test
   (:require (ga [main :as ga])) 
  (:gen-class )) 
  
;example usage
(defn -main [args]
  (let [target (vec "Hello World!")]
    (defn get-fitness 
      "custom comparator function for evaluating fitness of members"
      [value target]
      (reduce - 0 (map #(if (= (first %1) (second %1)) 0 1) 
        (ga/zip value target))))       
    (defn mutator 
      "member value generator"
      []
      (char (ga/rand-in-range 32 126)))         
    ;run the evolution function and print the result  
    (time (println (apply str (:value @(first (ga/evolve 500 0.01 target mutator get-fitness))))))))
  
(-main nil)