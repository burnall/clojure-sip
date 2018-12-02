(ns sip.advent-of-code-2019)

(def input1
  (-> "src/sip/adv-input1.txt"
      (slurp)
      (clojure.string/split #"\n")
      ((fn [s] (map #(Integer/parseInt %) s)))))
 

(defn adv1 [] 
  (reduce + input1))

(defn find-non-unique [occur [x & xs]]
  (if (occur x)
    (lazy-seq (cons x (find-non-unique occur xs)))
    (recur (conj occur x) xs))) 
  

(defn adv2 
  ([] (adv2 input1))  
  ([s]
    (->> s
         (cycle)
         (reductions + 0)
         (find-non-unique #{})
         (take 10))))
         
