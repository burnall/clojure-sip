(ns sip.veitch2)

;#{'a 'B 'C 'd} => [0110]
(defn toIntArray [set] 
  (mapv (comp #(if (some? %) 1 0) set) 
        (take (count set) ['A 'B 'C 'D])))

;[0 1 1 0] [0 1 0 0] => {:diff 1 :res [0 1 2 0]}
(defn _glue [termA termB]
  (reduce (fn [{diff :diff res :res} [a b]] 
             (if (= a b)
                {:diff diff :res (conj res a)}
                {:diff (inc diff) :res (conj res 2)}))
          {:diff 0 :res []}
          (map vector termA termB)))  

(defn glue[termA termB]
  (let [g (_glue termA termB)]
    (when (= (:diff g) 1) 
          (:res g))))    

(defn match[terms]
  (for [termA terms 
        termB terms
        :when (< (:id termA) (:id termB))
        :let [g (glue (:data termA) (:data termB))]
        :when (some? g)]
    {:data g :parents [(:id termA) (:id termB)]}))

(defn addId[terms] 
  (mapv (fn[id term] (assoc term :id id))
       (iterate inc 0) 
       terms))

(defn cover? [generic term]
  (every? (fn [[a b]] (or (= a 2) (= a b)))
          (map vector generic term)))

;[{:term term, :covers [id1...]}]
(defn findIrreducibles[solution]
  22
)	

(defn vei [termSet]
  (let [terms (mapv (comp (fn[term] {:data term}) toIntArray) 
                       termSet) 
        terms (addId terms)]
    (do (print terms "\n\n")
        (take-while (comp seq :next) 
          (iterate (fn [{next :next solution :solution}]
                     {:next (addId (match next)) :solution (conj solution next)})
                   {:next terms :solution [terms]})))))  

(defn vei-ex []
  (vei #{#{'a 'b 'c} 
         #{'a 'B 'c}
         #{'a 'b 'C}
         #{'a 'B 'C}}))


(defn vei-ex2 []
  (vei #{#{'a 'b 'c} 
         #{'a 'B 'c}
         #{'a 'B 'C}}))

(defn vei-ex3 []
  (vei #{#{'a 'B 'C 'd}
         #{'A 'b 'c 'd}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}
         #{'A 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'B 'C 'd}}))
                                                                        

