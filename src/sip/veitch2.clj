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
                ;(do (print "derived term:\n" derivedTerm "covered:\n" covered "\n")
                  {:notCoveredTerms (clojure.set/difference notCoveredTerms covered)
                   :minTerms (conj minTerms derivedTerm)}
                ;(do (print "derived term:\n" derivedTerm "\n") 
                  {:notCoveredTerms notCoveredTerms :minTerms minTerms}))

            {:notCoveredTerms originalTerms
             :minTerms #{}}

             derivedTerms)))

(defn findMinCoverage2 [originalTerms derivedTerms]
  (let [getDerived (fn [original] 
                      (filterv (fn [derived] (covers? derived original)) derivedTerms))
        covers (map getDerived originalTerms)]
     covers))


(defn vei0 [termSet]
  (let [originalTerms (set (map toIntArray termSet))
        terms (addId (mapv (fn[term] {:data term}) originalTerms))
        sol (buildSolution [terms])
        irr (findIrreducibles sol)
        minTerms (findMinCoverage originalTerms irr)]
    (do (print "Input:\n" terms "\n\n")
        (print "Solution:\n" sol "\n\n")
        (print "Irreducibles:\n" irr "\n\n")
        (print "Min terms:\n" minTerms "\n\n")
        (set (map toLetterSet minTerms))
        )))  

(defn vei [termSet]
  (let [originalTerms (set (map toIntArray termSet))
        terms (addId (mapv (fn[term] {:data term}) originalTerms))
        sol (buildSolution [terms])
        irr (findIrreducibles sol)
        minTerms (findMinCoverage2 originalTerms irr)]
    minTerms))  

(defn vei-ex0 []
  (vei #{#{'a 'b 'c} 
         #{'a 'B 'C}
         #{'A 'B 'C}}))

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

(defn vei-ex4 []
  (vei #{#{'a 'b 'c} 
         #{'a 'B 'c}
         #{'a 'b 'C}
         #{'a 'B 'C}}))

(defn vei-ex5 []
  (vei #{#{'A 'B 'C 'D}
         #{'A 'B 'C 'd}}))

(defn vei-ex6 []
  (vei #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'C 'd}
         #{'A 'B 'C 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}}))

(defn vei-ex7 []
  (vei #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'b 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'd}}))
                                                                        

