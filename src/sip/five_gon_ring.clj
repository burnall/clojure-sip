(ns sip.five-gon-ring)

;68

(defn try-solve [base] (let [sum (+ 10 (base 0) (base 1))]
                         (conj base
                               (- sum (base 1) (base 2))
                               (- sum (base 2) (base 3))
                               (- sum (base 3) (base 4))
                               (- sum (base 4) (base 0))
                               10)))

(defn bit [numb] (bit-set 0 (dec numb)))

(defn is-correct [sol] (= 1023 (reduce bit-or (map bit sol))))

(defn digits [n] (let [d0 (mod n 10) n0 (quot n 10)
                       d1 (mod n0 10) n1 (quot n0 10)
                       d2 (mod n1 10) n2 (quot n1 10)
                       d3 (mod n2 10) n3 (quot n2 10)
                       d4 (mod n3 10)]
                   (vec (map inc [d4 d3 d2 d1 d0]))))

(defn all-distinct-digits [v mask ind] (let [b (bit (v ind))]
                                         (cond (not= 0 (bit-and mask b)) false
                                               (= ind 0) (= (bit-and mask b) 0)
                                               :else (recur v (bit-or mask b) (dec ind)))))

(defn normalize [v] (let [items [[(v 9) (v 0) (v 1) 0]
                                   [(v 5) (v 1) (v 2) 1]
                                   [(v 6) (v 2) (v 3) 2]
                                   [(v 7) (v 3) (v 4) 3]
                                   [(v 8) (v 4) (v 0) 4]]
                          minItem (reduce #(if (< (first %1) (first %2)) %1 %2) items)
                          i (last minItem)]
                      [(items i) (items (mod (+ i 1) 5)) (items (mod (+ i 2) 5))
                       (items (mod (+ i 3) 5))  (items (mod (+ i 4) 5))]
                      ))

(defn stringify [items] (reduce str (map #(reduce str (take 3 %1)) items)))


(def candidates (filter #(all-distinct-digits %1 0 4) (map digits (range 1234 87655))))
(println (map stringify (map normalize (filter is-correct (map try-solve candidates)))))
;(println (map normalize (filter is-correct (map try-solve candidates))))
