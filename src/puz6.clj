(ns puz6
  (:require [clojure.string :as str]))

;Squares Squared

(defn ss[a b]
  (let
    [s (apply str (take-while #(<= % b) (iterate #(* % %) a)))
     l (count s)
     n (int (Math/ceil (Math/sqrt l)))
     t (vec (str s (apply str (repeat (- (* n n) l) \*))))
     f (fn[a i j v] (assoc a i (assoc (a i) j v)))
     ma (#(repeat % (repeat % \space)) (dec (* 2 n)))
     d [[1 1] [-1 1] [-1 -1] [1 -1]]
     ] [s l n t]
  ))


(println (ss 4 20))


;; graph 1
;(defn g[s]
;  (println (clojure.string/split s "\d+"))
;  )
