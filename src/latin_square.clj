(ns latin-square)

; get max row's width
; get all combinations
; seek squares in every combination
;  try every square of size 2, 3, ... with left top in the specific point
; distinct and count by size
(defn count-ls [s]
  (let [
        maxWidth (apply max (map count s))
        allMoves (fn [v n]
                   (map #(vec (concat (repeat % 0) v (repeat (- n % (count v)) 0)))
                        (range (- (inc n) (count v)))))
        product (fn [vs v] (for [va vs i v] (conj va i)))
        allCombinations (reduce product [[]] (map #(allMoves % maxWidth) s))
        ]
    allCombinations
  ))

;(println (count-ls [[1 2 3] [2 3 1 2 1] [3 1 2]]))
(println (count-ls [[1 2 3] [:a]]))
