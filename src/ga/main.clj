(ns ga)

(defn rand-in-range
  "generates a random value within the given range"
  [min max]
  (int (+ (* (Math/random) (inc (- max min))) min)))

(defn- mutate 
  "randomly mutates values in members of the population using the mutator function"
  [population mutator threshold]
  (for [member population]
    (update-in member [:value] #(if (< (rand) threshold) (map mutator %) %))))

(defn- rank [population fitness target]   
  (->>
    (for [member population]    
      (assoc-in member [:fitness] (fitness target (:value member)) ))
    (sort-by :fitness)    
    reverse))

(defn- fit? [population]
  (not-empty (filter #(= (:fitness %) 0) population)))

(defn- update-vals [{v1 :value} {v2 :value}]
  (let [[f1 f2] (if (> (rand) 0.5) [take drop] [drop take])
        [v1 v2] (if (> (rand) 0.5) [v1 v2] [v2 v1])]
    {:fitness nil
     :value (concat (f1 (/ (count v1) 2) v1) 
                    (f2 (/ (count v2) 2) v2))}))

(defn- mate [members]
  (apply map update-vals (split-at (/ (count members) 2) members)))

(defn- evolve-step [population mutator threshold fitness target]
  (let [mutated (rank (mutate population mutator threshold) fitness target)
        size    (count mutated)
        promote-size (/ size 5)
        keep-size (- (/ size 2) promote-size)
        [xs ys] (split-at keep-size mutated)]
    (concat xs (take promote-size ys) (mate mutated))))

(defn- gen-member [mutator target]
  {:fitness nil :value (take (count target) (repeatedly #(mutator nil)))})

(defn- init-population [size mutator target]
  (take size (repeatedly #(gen-member mutator target))))

(defn evolve [size threshold target mutator fitness]
  (loop [population (rank (init-population size mutator target) fitness target)]
    (if (fit? population) 
      population 
      (recur (evolve-step population mutator threshold fitness target)))))