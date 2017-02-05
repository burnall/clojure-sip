(ns latin-square)

; get max row's width
; get all combinations
; seek squares in every combination
;  try every square of size 2, 3, ... with left top in the specific point
; distinct and count by size
(defn count-ls-first [s]
  (let [
        maxX (apply max (map count s))
        maxY (count s)
        allMoves (fn [v n]
                   (map #(vec (concat (repeat % 0) v (repeat (- n % (count v)) 0)))
                        (range (- (inc n) (count v)))))
        product (fn [vs v] (for [va vs i v] (conj va i)))
        allCombinations (reduce product [[]] (map #(allMoves % maxX) s))
        pick (fn[m x y a] (mapv #(subvec (m %) x (+ x a)) (range y (+ y a))))
        allSquares (fn[sq] (for [x (range maxX) y (range maxY) a (range 2 (inc (min (- maxX x) (- maxY y))))] (pick sq x y a)))
        trans (partial apply map vector)
        latin? (fn[sq] (let [sample (set (filter #(not= 0 %) (sq 0)))]
                         (and (= (count sq) (count sample))
                               (every? #(= (set %) sample) sq)
                               (every? #(= (set %) sample) (trans sq)))))
        ]
    (into {} (map (fn[[a b]] [a (count b)]) (group-by count (distinct (filter latin? (mapcat allSquares allCombinations))))))
  ))

; Try idea with no materialisation of combinations. Skip now for opt2 sake
(defn count-ls-opt [s]
  (let [
        maxX (apply max (map count s))
        maxY (count s)
        allMoves (map #(range (inc (- maxX (count %)))) s)
        allCombinations (reduce (fn[vs v] (mapcat (fn[vi] (map #(conj vi %) v)) vs)) [[]] allMoves)
        checkLatin nil
        findLatin (filter some? (map checkLatin allCombinations))
        ]
    allCombinations
    ))


(defn count-ls-opt2 [s]
  (let [
        maxX (apply max (map count s))
        allMoves (fn [v n]
                   (map #(vec (concat (repeat % 0) v (repeat (- n % (count v)) 0)))
                        (range (- (inc n) (count v)))))
        trans (partial apply map vector)
        latin? (fn[sq] (let [sample (set (filter #(not= 0 %) (sq 0)))]
                         (and (= (count sq) (count sample))
                                                  (every? #(= (set %) sample) sq)
                                                  (every? #(= (set %) sample) (trans sq)))))
        product (fn [vs v] (for [va vs i v] (conj va i)))
        pick (fn[m x y a] (mapv #(subvec (m %) (- x a) (inc x)) (range (- y a) (inc y))))
        allSquares (fn[sq]
                     (let [maxY (count sq)]
                       (for [x (range 1 maxX) a (range 1 (min (inc x) maxY))] (pick sq x (dec maxY) a))))
        makeAndCheck (fn [{:keys [:vs :ls]} v]
                       (let [ms (allMoves v maxX) p (product vs ms)]
                         {:vs p :ls (vec (concat ls (filter latin? (mapcat allSquares p))))}))
        findLatin (reduce makeAndCheck {:vs [[]] :ls []} s)

        ]
    (into {} (map (fn[[a b]] [a (count b)]) (group-by count (distinct (:ls findLatin)))))
    ))


(def count-ls count-ls-opt2)

(println (count-ls '[[A B C D]
                     [A C D B]
                     [B A D C]
                     [D C A B]]))

(println (count-ls '[[A B C D E F]
                     [B C D E F A]
                     [C D E F A B]
                     [D E F A B C]
                     [E F A B C D]
                     [F A B C D E]]))

(println (count-ls '[[A B C D]
                     [B A D C]
                     [D C B A]
                     [C D A B]]))

(println (count-ls '[[B D A C B]
                     [D A B C A]
                     [A B C A B]
                     [B C A B C]
                     [A D B C A]]))

(println (count-ls [  [2 4 6 3]
                    [3 4 6 2]
                    [6 2 4]  ]))

(println (count-ls [[1]
                    [1 2 1 2]
                    [2 1 2 1]
                    [1 2 1 2]
                    []       ]))

(println (count-ls  [[3 1 2]
                     [1 2 3 1 3 4]
                     [2 3 1 3]    ]))

(println (count-ls [[8 6 7 3 2 5 1 4]
                    [6 8 3 7]
                    [7 3 8 6]
                    [3 7 6 8 1 4 5 2]
                    [1 8 5 2 4]
                    [8 1 2 4 5]]))

(println (count-ls [[1 2 3] [2 3 1 2 1] [3 1 2]]))