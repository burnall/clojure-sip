(ns sip.fractions)


(defn next-fraction [d acc] (if (zero? acc) d (+ d (/ 1 acc))))
(defn nth-fraction [f n acc]
  (if (< n 0)
    acc
    (recur f (dec n) (next-fraction (f n) acc))))

; #66
(defn get-period [n]
  (let [root (int (Math/sqrt n))]

    (letfn [(get-next [[p a b]]
              (let [b1 (quot (- n (* a a)) b)
                    p1 (quot (+ root a) b1)
                    a1 (- (* p1 b1) a)]
                [p1 a1 b1]))

            (iter [items]
              (let [new-item (get-next (peek items))
                    pos (.indexOf items new-item)]
                (if (not= -1 pos) [(- (count items) pos) items] (recur (conj items new-item)))))]

      (iter [[root, root, 1N]]))))

(defn solve-pell [n]
  (let [[period items] (get-period n)
        m (cond (= 1 period) 1
                (odd? period) (- (* 2 period) 1)
                :else (dec period))
        f (nth-fraction #(let [ind (if (zero? %1) 0 (+ 1 (mod (dec %1) period)))] (first (items ind))) m 0)]
    (if (integer? f) f (numerator f))
    ))

(let [numbers (filter #(let [sq (int (Math/sqrt %1))]
                        (not= %1 (* sq sq)))
                      (range 1 1001))]
      (println (reduce #(if (> (first %1) (first %2)) %1 %2) (map #(-> [(solve-pell %1), %1]) numbers)))
      )

(def aa [3 1 1 1 1 6 1 1 1 1])
(println (nth-fraction #(aa %1) 9 0))

