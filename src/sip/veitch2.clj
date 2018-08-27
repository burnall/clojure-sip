(ns sip.veitch2)

;#{'a 'B 'C 'd} => [0110]
(defn to-int-array [set] 
  (mapv (comp #(if (some? %) 1 0) set) 
        (take (count set) ['A 'B 'C 'D])))

;[0110] => #{'a 'B 'C 'd}
(defn to-letter-set [bits]
  (set
   (mapcat (fn [bit t f] (condp = bit 0 [f] 1 [t] 2 []))
           bits 
           ['A 'B 'C 'D] 
           ['a 'b 'c 'd])))

;[0 1 1 0] [0 1 0 0] => {:diff 1 :res [0 1 2 0]}
(defn _glue [term-a term-b]
  (reduce (fn [{diff :diff res :res} [a b]] 
             (if (= a b)
                {:diff diff, :res (conj res a)}
                {:diff (inc diff), :res (conj res 2)}))
          {:diff 0, :res []}
          (map vector term-a term-b)))  

(defn glue[term-a term-b]
  (let [g (_glue term-a term-b)]
    (when (= (:diff g) 1) 
          (:res g))))    

(defn match[terms]
  (for [term-a terms 
        term-b terms
        :when (< (:id term-a) (:id term-b))
        :let [g (glue (:data term-a) (:data term-b))]
        :when (some? g)]
    {:data g, :parents [(:id term-a) (:id term-b)]}))

(defn add-id[terms] 
  (mapv (fn[id term] (assoc term :id id))
        (range) 
        terms))

(defn build-solution[solution-in-progress]
  (if-let [next (seq (match (last solution-in-progress)))]
    (recur (conj solution-in-progress (add-id next))) 
    solution-in-progress))

;{:data [1 0 2 1], :parents [0 5], :id 1}
(defn reducible?[solution level id]
  (and (< (inc level) (count solution))
       (some (fn [{parents :parents}] (some #(= id %) parents)) 
             (solution (inc level)))))

;[{:term term, :covers [id1...]}]
(defn find-irreducibles [solution]
  (set
   (mapcat (fn [level] 
             (map :data 
                  (filter #(not (reducible? solution level (:id %)))
                          (solution level))))
           (range 0 (count solution)))))

(defn covers? [generic term]
  (every? (fn [[a b]] (or (= a 2) (= a b)))
          (map vector generic term)))

(defn find-min-coverage [original-terms derived-terms]
  (let [get-derived-coverage-for-term 
         (fn [term] (set (filter (fn [derived] (covers? derived term)) derived-terms)))

        cover-map (into {} (mapv (fn[term] [term (get-derived-coverage-for-term term)]) original-terms))

        get-min-of-two (fn [[term-a cover-a] [term-b cover-b]]
                       (if (< (count cover-b) (count cover-a)) [term-b cover-b] [term-a cover-a]))
        find-min (fn [cover-map] (reduce get-min-of-two cover-map))

        remove-derived (fn [cover-map derived-term]
          (into {} (filter (fn [[term cover]] (not (contains? cover derived-term))) cover-map)))

        _find-min-coverage 
          (fn [acc cover-map] 
            (let [[term cover] (find-min cover-map) 
                  derived (first cover)
                  new-cover-map (remove-derived cover-map derived)
                  new-acc (conj acc derived)]
              (if (zero? (count new-cover-map))
                new-acc
                (recur new-acc new-cover-map))))]
    (_find-min-coverage #{} cover-map)))   
     
(defn vei [term-set]
  (let [original-terms (set (map to-int-array term-set))
        terms (add-id (mapv (fn[term] {:data term}) original-terms))
        sol (build-solution [terms])
        irr (find-irreducibles sol)
        min-terms (find-min-coverage original-terms irr)]
    (set (map to-letter-set min-terms)))) 
