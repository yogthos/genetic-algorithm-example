(defn mutator [c] 
  (if c
    (let [new-val ((if (> (rand) 0.5) + -)
                    (rand-int 5) (int c))]
      (char (cond 
              (> new-val 126) 126
              (< new-val 32) 32
              :else new-val)))
    (char (rand-in-range 32 126))))

(defn fitness [value target]
  (reduce 
    (fn [rank [a b]] (if (= a b) rank (dec rank)))
    0 (map vector value target)))

(defn -main [string]
  (let [target  (vec string)]    
    (time (println (apply str (:value (first (evolve 1000 0.01 mutator fitness target))))))))

(-main "Hello World!")
