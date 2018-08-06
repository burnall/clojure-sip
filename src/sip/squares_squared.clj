(ns sip.squares-squared
  (:require [clojure.string :as str]))

;Squares Squared

(defn ss [a b]
  (let
    [s (apply str (take-while #(<= % b) (iterate #(* % %) a)))
     l (count s)
     n (int (Math/ceil (Math/sqrt l)))
     t (vec (str s (apply str (repeat (- (* n n) l) \*))))
     f (fn [a i j v] (assoc a i (assoc (a i) j v)))
     ma (vec (#(repeat % (vec (repeat % \space))) (dec (* 2 n))))
     d [[1 1] [-1 1] [-1 -1] [1 -1]]
     mid [(dec n) (+ n -2 (mod n 2))]
     ps (first (reduce (fn [[ps [a b] grad step sc fst] i]
                         (let [[grad1 step1 sc1 fst1]
                               (cond (< step sc) [grad (inc step) sc fst]
                                     fst [(inc grad) 1 sc false]
                                     :else [(inc grad) 1 (inc sc) true])
                               [dx dy] (d (mod grad1 4))]
                           [(conj ps [a b i]) [(+ a dx) (+ b dy)] grad1 step1 sc1 fst1]))
                       [[] mid 0 0 1 true] t))
     ] (map (partial apply str) (reduce (fn[m [i j v]] (f m j i v)) ma ps))
    )
  )


(map println (ss 5 5000))

