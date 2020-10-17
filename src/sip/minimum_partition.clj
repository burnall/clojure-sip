(ns sip.minimum-partition)

(defn walkdown [n [new-solutions [best-v best-goal]] [v goal]] 
  (let [new-solutions (conj new-solutions [v goal])
        new-goal (- goal n)]
    (if (< new-goal 0)
      [new-solutions [best-v best-goal]]
      (let [sol [(conj v n) new-goal]
            new-best (if (< new-goal best-goal) 
                        sol
                        [best-v best-goal])]
        [(conj new-solutions sol) new-best]))))  

(defn solve
  ([[n & others] solutions best] 
    (if n
       (let [[new-solutions new-best] 
              (reduce (partial walkdown n)  
                      [[] best]
                      solutions)]
         (recur others new-solutions new-best))               
       best))

  ([coll] 
    (let [half (/ (reduce + coll) 2)
          sol [[] half]] 
      (solve (sort-by - coll) 
             [sol]
             sol)))) 
  

