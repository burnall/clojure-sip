(ns sip.veitch)

(defn vee [s]
  (let [
        gb #(if % 1 0)
        tov (fn [s] [(gb (s 'A)) (gb (s 'B)) (gb (s 'C)) (gb (s 'D))])
        cmp (fn [a b]
              (reduce (fn [[s cnt] [x y]]
                        (cond (= x y) [(conj s x) cnt]
                              (>= cnt 1) [nil 2]
                              :else [(conj s 2) (inc cnt)]))
                      [[] 0]
                      (map #(-> [% %2]) a b)))
        ]
  (println (cmp (tov #{'a 'B 'C 'D}) (tov #{'a 'b 'C 'd})))))

(vee 1)

(defn ve[a] )

(println (ve #{#{'A 'B 'C 'D}
               #{'A 'B 'C 'd}}))

(println (= (ve #{#{'a 'B 'C 'd}
                  #{'A 'b 'c 'd}
                  #{'A 'b 'c 'D}
                  #{'A 'b 'C 'd}
                  #{'A 'b 'C 'D}
                  #{'A 'B 'c 'd}
                  #{'A 'B 'c 'D}
                  #{'A 'B 'C 'd}})
            #{#{'A 'c}
              #{'A 'b}
              #{'B 'C 'd}}))


(println (= (ve #{#{'A 'B 'C 'D}
                  #{'A 'B 'C 'd}})
            #{#{'A 'B 'C}}))

(println (= (ve #{#{'a 'b 'c 'd}
                  #{'a 'B 'c 'd}
                  #{'a 'b 'c 'D}
                  #{'a 'B 'c 'D}
                  #{'A 'B 'C 'd}
                  #{'A 'B 'C 'D}
                  #{'A 'b 'C 'd}
                  #{'A 'b 'C 'D}})
            #{#{'a 'c}
              #{'A 'C}}))

(println (= (ve #{#{'a 'b 'c}
                  #{'a 'B 'c}
                  #{'a 'b 'C}
                  #{'a 'B 'C}})
            #{#{'a}}))

(println (= (ve #{#{'a 'B 'c 'd}
                  #{'A 'B 'c 'D}
                  #{'A 'b 'C 'D}
                  #{'a 'b 'c 'D}
                  #{'a 'B 'C 'D}
                  #{'A 'B 'C 'd}})
            #{#{'a 'B 'c 'd}
              #{'A 'B 'c 'D}
              #{'A 'b 'C 'D}
              #{'a 'b 'c 'D}
              #{'a 'B 'C 'D}
              #{'A 'B 'C 'd}}))

(println (= (ve #{#{'a 'b 'c 'd}
                  #{'a 'B 'c 'd}
                  #{'A 'B 'c 'd}
                  #{'a 'b 'c 'D}
                  #{'a 'B 'c 'D}
                  #{'A 'B 'c 'D}})
            #{#{'a 'c}
              #{'B 'c}}))


(println (= (ve #{#{'a 'B 'c 'd}
                  #{'A 'B 'c 'd}
                  #{'a 'b 'c 'D}
                  #{'a 'b 'C 'D}
                  #{'A 'b 'c 'D}
                  #{'A 'b 'C 'D}
                  #{'a 'B 'C 'd}
                  #{'A 'B 'C 'd}})
            #{#{'B 'd}
              #{'b 'D}}))

(println (= (ve #{#{'a 'b 'c 'd}
                  #{'A 'b 'c 'd}
                  #{'a 'B 'c 'D}
                  #{'A 'B 'c 'D}
                  #{'a 'B 'C 'D}
                  #{'A 'B 'C 'D}
                  #{'a 'b 'C 'd}
                  #{'A 'b 'C 'd}})
            #{#{'B 'D}
              #{'b 'd}}))