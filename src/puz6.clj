(ns puz6
  (:require [clojure.string :as str]))

;Squares Squared

(defn ss [a b]
  (let
    [s (apply str (take-while #(<= % b) (iterate #(* % %) a)))
     l (count s)
     n (int (Math/ceil (Math/sqrt l)))
     t (vec (str s (apply str (repeat (- (* n n) l) \*))))
     f (fn [a i j v] (assoc a i (assoc (a i) j v)))
     ma (#(repeat % (repeat % \space)) (dec (* 2 n)))
     d [[1 1] [-1 1] [-1 -1] [1 -1]]
     mid (+ n -2 (mod n 2))
     ]
    (reduce (fn [[[a b] ps grad step sc fst] i]
              (let [[grad1 step1 sc1 fst1]
                    (cond (< step sc) [grad (inc step) sc fst]
                          fst [(inc grad) 1 sc false]
                          :else [(inc grad) 1 (inc sc) true])
                    [dx dy] (d (mod grad1 4))]
                [[(+ a dx) (+ b dy)] (conj ps [a b i]) grad1 step1 sc1 fst1]))
            [[mid mid] [] 0 0 1 true] t)
    )
  )


(println (ss 2 256))


;; graph 1
;(defn g[s]
;  (println (clojure.string/split s "\d+"))
;  )
