(ns ga)

(defn rand-in-range
  "generates a random value within the given range"
  [min max]
  (int (+ (* (Math/random) (inc (- max min))) min)))

(defn- mutate 
  "randomly mutates values in members of the population using the mutator function"
  [population mutator threshold fitness target]
  (for [member population]
    (if (< (rand) threshold)
      (let [value (map mutator (:value member))]
        {:value value
         :fitness (fitness value target)})
      member)))

(defn- rank [population]
  (reverse (sort-by :fitness population)))

(defn- update-vals [fitness target {v1 :value} {v2 :value}]  
  (let [value (map #(if (> (rand) 0.5) %1 %2) v1 v2)]
    {:value value :fitness (fitness value target)}))

(defn- mate [population fitness target]
  (apply map 
         (partial update-vals fitness target) 
         (split-at (/ (count population) 2) population)))

(defn- evolve-step [size population mutator threshold fitness target]
  (let [mutated (rank (mutate population mutator threshold fitness target))        
        promote-size (/ size 5)
        keep-size (- (/ size 2) promote-size)
        [xs ys] (split-at keep-size mutated)]
    (concat xs (take promote-size ys) (mate mutated fitness target))))

(defn- gen-member [mutator fitness target]
  (let [value (take (count target) (repeatedly #(mutator nil)))] 
    {:value value :fitness (fitness value target)}))

(defn- init-population [size mutator fitness target]
  (rank (take size (repeatedly #(gen-member mutator fitness target)))))

(defn evolve [size threshold mutator fitness target]
  (loop [population (init-population size mutator fitness target)]
    (println (first population))
    (if (zero? (:fitness (first population))) 
      population 
      (recur (evolve-step size population mutator threshold fitness target)))))
