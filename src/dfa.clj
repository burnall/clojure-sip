(ns dfa)

(defn dfa [o]
  (let [{tr :transitions accepts :accepts start :start} o
        mv (partial mapcat (fn [[q s]] (map (fn [[c q2]] [q2 (str s c)]) (tr q))))
        f (fn f [pairs]
            (let [np (mv pairs)]
              (if (empty? np)
                []
                (lazy-seq (concat (distinct (map second (filter #(accepts (first %)) np))) (f np))))))
        ]
    (println (take 5 (f [[start ""]])))
    ))



;(defn f
;  ([] (f 1))
;  ([n] (lazy-seq (concat (repeat n n) (f (inc n))))))


(dfa '{:states      #{q0 q1 q2 q3}
       :alphabet    #{a b c}
       :start       q0
       :accepts     #{q1 q2 q3}
       :transitions {q0 {a q1}
                     q1 {b q2}
                     q2 {c q3}}})

(dfa '{:states      #{q0 q1 q2 q3 q4 q5 q6 q7}
       :alphabet    #{e h i l o y}
       :start       q0
       :accepts     #{q2 q4 q7}
       :transitions {q0 {h q1}
                     q1 {i q2, e q3}
                     q3 {l q5, y q4}
                     q5 {l q6}
                     q6 {o q7}}})

(fn[o]
  (let [{tr :transitions accepts :accepts start :start} o
        mv (partial mapcat (fn [[q s]] (map (fn [[c q2]] [q2 (str s c)]) (tr q))))
        f (fn f [pairs]
            (let [np (mv pairs)]
              (if (empty? np)
                []
                (lazy-seq (concat (distinct (map second (filter #(accepts (first %)) np))) (f np))))))
        ]
    (f [[start ""]])))