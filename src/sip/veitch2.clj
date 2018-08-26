(ns sip.veitch2)

;#{'a 'B 'C 'd} => [0110]
(defn toIntArray [set] 
  (mapv (comp #(if (some? %) 1 0) set) 
        (take (count set) ['A 'B 'C 'D])))

;[0110] => #{'a 'B 'C 'd}
(defn toLetterSet [bits]
  (set
    (mapcat 
      (fn [bit t f] (condp = bit 0 [f] 1 [t] 2 []))
      bits 
      ['A 'B 'C 'D] 
      ['a 'b 'c 'd])))

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
       (range) 
       terms))

(defn buildSolution[solutionInProgress]
  (if-let [next (seq (match (last solutionInProgress)))]
    (recur (conj solutionInProgress (addId next))) 
    solutionInProgress))

;{:data [1 0 2 1], :parents [0 5], :id 1}
(defn isReducible[solution level id]
  (and (< (inc level) (count solution))
       (some (fn [{parents :parents}] (some #(= id %) parents)) 
             (solution (inc level)))))

;[{:term term, :covers [id1...]}]
(defn findIrreducibles [solution]
  (set
    (mapcat
      (fn [level] 
        (map :data 
          (filter #(not (isReducible solution level (:id %)))
                  (solution level))))
      (range 0 (count solution)))))

(defn covers? [generic term]
  (every? (fn [[a b]] (or (= a 2) (= a b)))
          (map vector generic term)))


(defn findMinCoverage [originalTerms derivedTerms]
  (:minTerms
    (reduce (fn [{notCoveredTerms :notCoveredTerms minTerms :minTerms} derivedTerm]
              (if-let [covered (seq (filter #(covers? derivedTerm %) notCoveredTerms))]
                  {:notCoveredTerms (clojure.set/difference notCoveredTerms covered)
                   :minTerms (conj minTerms derivedTerm)}
                  {:notCoveredTerms notCoveredTerms :minTerms minTerms}))

            {:notCoveredTerms originalTerms
             :minTerms #{}}

             derivedTerms)))

(defn findMinCoverage2 [originalTerms derivedTerms]
  (let [getDerivedCoverageForTerm 
         (fn [term] (set (filter (fn [derived] (covers? derived term)) derivedTerms)))

        coverMap (into {} (mapv (fn[term] [term (getDerivedCoverageForTerm term)]) originalTerms))

        getMinOfTwo (fn [[termA coverA] [termB coverB]]
                       (if (< (count coverB) (count coverA)) [termB coverB] [termA coverA]))
        findMin (fn [coverMap] (reduce getMinOfTwo coverMap))

        removeDerived (fn [coverMap derivedTerm]
          (into {} (filter (fn [[term cover]] (not (contains? cover derivedTerm))) coverMap)))

        _findMinCoverage 
          (fn [acc coverMap] 
            (let [[term cover] (findMin coverMap) 
                 derived (first cover)
                 newCoverMap (removeDerived coverMap derived)
                 newAcc (conj acc derived)]
              (if (zero? (count newCoverMap))
                newAcc
                (recur newAcc newCoverMap))))    

        ]
    (_findMinCoverage #{} coverMap)))   
     
(defn vei [termSet]
  (let [originalTerms (set (map toIntArray termSet))
        terms (addId (mapv (fn[term] {:data term}) originalTerms))
        sol (buildSolution [terms])
        irr (findIrreducibles sol)
        minTerms (findMinCoverage2 originalTerms irr)]
    (set (map toLetterSet minTerms)))) 

                                                                        

