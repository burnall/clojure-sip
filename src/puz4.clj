(ns puz4)
(require 'clojure.set)

;130

(defn rp [n g]
  (let
    [fi (fn f[p s]
          (let [[a & b] s]
            (if (= a n)
              p
              (some (fn [[i t]] (f (conj p i) t)) (map-indexed #(list (inc %1) %2) b)))))]
    (fi [] g)
    ))

(rp 'd '(a
          (b
            (c)
            (d)
            (e))
          (f
            (g)
            (h))))

;(println (rp 'd '(a (b (c) (d) (e)) (f (g) (h)))))

;135

(defn calc [[a b c & r]]
  (if b (recur (cons (b a c) r)) a))

(fn [& p]
  (letfn [(f [[a b c & r]] (if b (recur (cons (b a c) r)) a))] (f p)))

;(println (calc0 2 + 5 * 3 - 10))

;137
(defn convert [n b]
  (let [f (fn f [m] (if (= m 0) [] (conj (f (quot m b)) (mod m b))))]
  (if (= n 0) [0] (f n))))


;(println (convert 99 99))

;132
(defn ins[p i xs]
  (first (reduce (fn[[acc a] b] (lazy-seq (if (seq acc) [(if (p a b) (conj acc i b) (conj acc b)) b] [[b] b])))
          [[] 0]  xs)))

(defn ins2[p i s]
  (if (seq s)
    (cons (first s) (mapcat (fn[a b] (if (p a b) [i b] [b])) s (rest s)))
    []
    )
  )

;(println (take 3 (ins2 < :less (range))))
;(println (ins2 < :less [1 6 7 4 3]))

;133
(defn superset [s]
  (if (seq s)
         (let [ss (superset (rest s))] (clojure.set/union ss (map #(conj % (first s)) ss)))
         #{#{}}
    ))

(defn sum-subsets [& s]
  (let [f (fn f[t]
        (if (seq t)
          (let [ss (f (rest t))] (clojure.set/union ss (map #(conj %1 (first t)) ss)))
          #{#{}}))
        g (fn[t] (map (partial apply +) t))]
    ;(map (comp g f) s)))
  (if (seq (apply clojure.set/intersection (map (comp set g (partial filter seq) f) s)))
    true
    false)))

;(println (superset [1 2 3]))
(println (sum-subsets #{-1 1 99} #{-2 2 888} #{-3 3 7777}))
;(println (clojure.set/intersection #{1 :a} #{:a 3} #{:a}))

