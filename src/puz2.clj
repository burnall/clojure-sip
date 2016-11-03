(ns puz2)

;89

(defn get-i [s e]
  (last (first (filter #(= e (first %)) (map-indexed #(-> [(last %2) %]) s)))))

(defn drop-i [s i]
  (vec (concat (subvec (vec s) 0 i) (subvec (vec s) (inc i)))))

(defn tour?
  ([g] (let [h (map-indexed #(conj %2 %) g)]
         (= true (tour? (mapcat (fn [[a b i]] [[a b i] [b a i]]) h) h))))
  ([s u]
   (if (seq u)
     (if (seq s)
       (some
         #{true}
         (map (fn [[_ b i]]
                (let [v (drop-i u (get-i u i))]
                  (tour? (mapcat (fn [[c d j]] (cond (= b c) [[c d j]] (= b d) [[d c j]])) v) v))) s))
       false)
     true)))

;(println (tour? [[1 2] [2 3] [4 0]]))
;(println (tour? [[:a :b] [:a :c] [:c :b] [:a :e]
;                 [:b :e] [:a :d] [:b :d] [:c :e]
;                 [:d :e] [:c :f] [:d :f]]))

;90


(defn conn? [g]
  (let
    [to-set (fn [s] (into #{} (mapcat identity s)))
     sz (count (to-set g))
     f (fn [s]
         (let [t (to-set (filter #(some s %) g)) z (count t)]
           (cond (= z sz) true
                 (= z (count s)) false
                 :else (recur t))))]
    (f #{(ffirst g)})))


;(println (conn? [[0 1] [2 3]]))


;92

;(clojure.string/replace "The color is red red" #"red" "blue")
;(count (re-seq #"and" "and 2 and 4"))


(defn ro [s m v]
  (if (seq s)
    (let [[a b] (first m)
          c (count (re-seq (re-pattern a) s))]
      (recur (clojure.string/replace s a "") (rest m) (+ v (* c b))))
    v))

(defn roman [s]
  (ro s '(["IV" 4] ["IX" 9] ["XL" 40] ["XC" 90] ["CD" 400] ["CM" 900]
           ["I" 1] ["V" 5] ["X" 10] ["L" 50] ["C" 100] ["D" 500] ["M" 1000]) 0))


;(println (roman "DCCCXXVII"))
;(println (roman "MMMCMXCIX"))

;93

(defn pf [s]
  (filter #(and (sequential? %) (every? (complement sequential?) %)) (tree-seq sequential? identity s)))

;(println (pf [[[[:a :b]]] [[:c :d]] [:e :f]]))

;94

(defn gl [s]
  (let
    [mi (count s) mj (count (s 0))
     ge (fn [i j] (get (s i) j))
     f (fn [i j] (if (or (< i 0) (= i mi) (< j 0) (= j mj)) [] [(ge i j)]))
     nb (fn [i j] (concat (f (dec i) (dec j)) (f (dec i) j) (f (dec i) (inc j)) (f i (dec j)) (f i (inc j)) (f (inc i) (dec j)) (f (inc i) j) (f (inc i) (inc j))))
     lv (fn [i j] (count (filter #(= \# %) (nb i j))))
     g (fn [i j c] (let [d (lv i j)] (if (or (= d 3) (and (= d 2) (= c \#))) \# \space)))]
    (map-indexed (fn [i s2] (apply str (map-indexed (fn [j c] (g i j c)) s2))) s)))
;(g 0 0 \#)))

;(println (gl ["      "
;              " ##   "
;              " ##   "
;              "   ## "
;              "   ## "
;              "      "]))

;95

(defn ibt [t]
  (and (coll? t) (= (count t) 3) (every? #(or (nil? %) (ibt %)) (rest t))))


;(println (ibt [2 nil nil]))
;(println (ibt [4 false nil]))

;97
(defn ps [i]
  (if (= 1 i)
    [1]
    (let [s (ps (dec i))]
      (concat [1] (map #(+ % %2) s (rest s)) [1]))))

;(println (ps 3))
;(println (ps 4))
;(println (ps 5))

;99
(defn digits [n]
  (reverse (map last
                (take-while
                  #(> (first %) 0)
                  (iterate (fn [[a b]] (let [m (/ (- a b) 10)] [m (mod m 10)])) [n (mod n 10)])))))

(fn x [a b]
  ((fn digits [n]
     (reverse (map last
                   (take-while
                     #(> (first %) 0)
                     (iterate (fn [[a b]] (let [m (/ (- a b) 10)] [m (mod m 10)])) [n (mod n 10)]))))) (* a b)))

;100

(defn g [a b] (if (= b 0) a (recur b (mod a b))))

(defn lcd [& s]
  (reduce #(/ (* % %2) (g % %2)) s))

(defn lcd [& s]
  (letfn [(g [a b] (if (= b 0) a (recur b (mod a b))))]
    (reduce #(/ (* % %2) (g % %2)) s)))

;101
#(let [m (memoize (fn [f a b]
                    (cond (empty? a) (count b) (empty? b) (count a)
                          :else (let [d (if (= (first a) (first b)) 0 1)
                                      x (rest a) y (rest b)]
                                  (min (inc (f f a y)) (inc (f f x b)) (+ d (f f x y)))))))]
  (m m % %2))

;102

(defn join [c s]
  (for [a c i s
        :when (not (a i))]
    (conj a i)))


(defn comb [n s]
  (if (> n (count s))
    #{}
    (letfn
      [(f [m]
         (if (= 0 m)
           #{#{}}
           (join (f (dec m)) s)))]
      (f n))))

;(println (comb 2 #{0 1 2 3}))


#(letfn
  [(j [c]
     (for [a c i %2 :when (not (a i))]
       (conj a i)))
   (f [m] (if (= 0 m) #{#{}} (j (f (dec m)))))]
  (if (> % (count %2))
    #{}
    (f %)))


;104

(defn fro [n]
  (let [s [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
        f (fn [a m d] [(mod a m) (apply str (repeat (int (/ a m)) d))])]
    (first (reduce (fn [[r a] [m d]] (let [[t k] (f a m d)] [(str r k) t]))
                   ["" n] s))))




;["IV" 4] ["IX" 9] ["XL" 40] ["XC" 90] ["CD" 400] ["CM" 900]
;["I" 1] ["V" 5] ["X" 10] ["L" 50] ["C" 100] ["D" 500] ["M" 1000]

;105

(defn ff [t] (apply hash-map (reduce (fn [s i] (if (keyword? i) (conj s i []) (let [p (peek s)] (conj (pop s) (conj p i))))) [] t)))

[:a 1 2 3 :b :c 4]

;106
(defn path [a b]
  ((fn [s n] (if (s b) n (recur (set (mapcat #(let [t [(* 2 %) (+ 2 %)]] (if (odd? %) t (conj t (/ % 2)))) s)) (inc n)))) #{a} 0))

;108

(defn fm [& s]
  (let [c (dec (count s))]
    (reduce
      (fn f [acc v]
        (if (seq v)
          (let [ff (first v) e (acc ff)]
            (if e (if (= e c) (reduced ff) (assoc acc ff (inc e))) (f (assoc acc ff 1) (rest v))))
          acc
          ))
      {} (apply map (fn [& u] u) s))))

(println (fm [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
(println (fm (range) (range 0 100 7/6) [2 3 5 7 11 13]))
(println (fm (range) (range) (range)))

(defn it [& colls]
  (let [step
        (fn step [cs]
          (lazy-seq
            (let [ss (map seq cs)]
              (when (some identity ss)
                (cons (map #(if % (first %) nil) ss) (step (map rest ss)))))))]
    (step colls)))


(defn fm [& s]
  (let [c (dec (count s))
        it (fn[& colls]
          (let [step
                (fn step [cs]
                  (lazy-seq
                    (let [ss (map seq cs)]
                      (when (some identity ss)
                        (cons (map #(if % (first %) nil) ss) (step (map rest ss)))))))]
            (step colls)))]
    (last (filter #(not (coll? %)) (reductions
      (fn f [acc v]
        (let [ff (first v) e (delay (acc ff)) r (rest v)]
          (cond
            (empty? v) acc
            (nil? ff) (f acc r)
            (not (coll? acc)) acc
            (or (= c 0) (= @e c)) ff
            @e (f (assoc acc ff (inc @e)) r)
            :else (f (assoc acc ff 1) r))))
      {} (apply it s))))))

(defn fm [& s]
  (let [c (dec (count s))
        it (fn[& colls]
             (let [step
                   (fn step [cs]
                     (lazy-seq
                       (let [ss (map seq cs)]
                         (when (some identity ss)
                           (cons (map #(if % (first %) nil) ss) (step (map rest ss)))))))]
               (step colls)))]
    (last (take 30 (reductions
            (fn f [acc v]
              (let [ff (first v) r (rest v)]
                (do (println v (mod (System/currentTimeMillis) 100000))
                (cond
                  (empty? v) acc
                  (nil? ff) (f acc r)
                  (not (coll? acc)) acc
                  (or (= c 0) (= (acc ff) c)) ff
                  (acc ff) (f (assoc acc ff (inc (acc ff))) r)
                  :else (f (assoc acc ff 1) r)))))
            (hash-map) (apply it s))))))


(defn fm [& s]
  (let [c (dec (count s))
        it (fn[& colls]
             (let [step
                   (fn step [cs]
                     (lazy-seq
                       (let [ss (map seq cs)]
                         (when (some identity ss)
                           (cons (map #(if % (first %) nil) ss) (step (map rest ss)))))))]
               (step colls)))]
    (last (take 30 (reductions
                     (fn f [acc v]
                       (do (println v (count acc) (mod (System/currentTimeMillis) 100000))
                       (let [ff (first v) r (rest v)]
                             (cond
                               (empty? v) acc
                               (nil? ff) (f acc r)
                               (not (coll? acc)) acc
                               (or (= c 0) (= (acc ff) c)) ff
                               (acc ff) (f (assoc acc ff (inc (acc ff))) r)
                               :else (f (assoc acc ff 1) r)))))
                     {} (apply it s))))))



(defn fm [& s]
  (let [c (dec (count s))
        it (fn[& colls]
             (let [step
                   (fn step [cs]
                     (lazy-seq
                       (let [ss (map seq cs)]
                         (when (some identity ss)
                           (cons (map #(if % (first %) nil) ss) (step (map rest ss)))))))]
               (step colls)))]


    (last (take 30 (reductions
                     (fn f [acc v]
                       (do (println v (count acc) (mod (System/currentTimeMillis) 100000))
                           (let [ff (first v) r (rest v)]
                             (cond
                               (empty? v) acc
                               (nil? ff) (f acc r)
                               (not (coll? acc)) acc
                               (or (= c 0) (= (acc ff) c)) ff
                               (acc ff) (f (assoc acc ff (inc (acc ff))) r)
                               :else (f (assoc acc ff 1) r)))))
                     {} (apply it s))))))