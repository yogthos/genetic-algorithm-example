(defn mutator [_] (char (rand-in-range 32 126)))

(defn fitness [value target]
  (reduce 
    (fn [rank [a b]] (if (= a b) rank (dec rank)))
    0 (map vector value target)))

(defn -main [string]
  (let [target  (vec string)]    
    (time (println (apply str (:value (first (evolve 1000 0.01 mutator fitness target))))))))

(-main "Hello World!")
