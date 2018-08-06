(ns sip.puzzles)

;(:use clojure.set)
;(println (clojure.set/difference #{2} #{2 3}))

(defn max-seq [xs]
  (let [m
        (loop [x (rest xs) cr [(first xs)] mx []]
          (if (empty? x)
            mx
            (let [f (first x)
                  nw (if (> f (last cr)) (conj cr f) [f])]
              (recur (rest x)
                     nw
                     (if (> (count nw) (count mx)) nw mx)))))]
    (if (= 1 (count m)) [] m)))

;(println (max-seq [1 0 1 2 3 0 4 5]))
;(println (max-seq [5 6 1 3 2 7]))
;(println (max-seq [2 3 3 4 5]))
;(println (max-seq [7 6 5 4]))

(defn part [xs, n]
  (second (reduce (fn [[cur acc] i]
                    (let [nw (conj cur i)]
                      (if (= (count nw) n) [[] (conj acc nw)] [nw acc])))
                  [[] []] xs)))

;(println (part [0 1 2 3 4 5 6 7] 5))

#(second (reduce (fn [[cur acc] i]
                   (let [nw (conj cur i)]
                     (if (= (count nw) %2) [[] (conj acc nw)] [nw acc])))
                 [[] []] %))

(reduce #({%2 %} ((set %) %2) (conj % %2)) [] [1 2 1 3 1 2 4])


;58
(defn mycomp [& a]
  (reduce #(fn [& b] (%2 (apply % b))) (reverse a)))

;(println ((mycomp rest reverse) [1 2 3 4]))

;59
(defn my-juxt [& a]
  (fn [& b] (map #(apply % b) a)))

;(println ((my-juxt + max min) 2 3 1 5))
;(println ((my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

;60
(defn sr
  ([f col] (sr f (first col) (rest col)))
  ([f init col]
   (cons init (lazy-seq (when-let [s (seq col)]
                          (sr f (f init (first s)) (rest s)))))))

;(println (take 5 (sr + (range))))
;(println (sr conj [1] [2 3 4]))
;(println (sr * 2 [3 4 5]))

;63
(defn gb [f s]
  (reduce #(update % (f %2) (fn [x] (vec (conj x %2)))) {} s))
;(reduce #(update % %2 identity) {} s))
;(println (gb #(mod % 3) [1 2 4 5 3 10]))

;67
(defn primes [n s]
  (lazy-seq
    (if (some #(= 0 (mod n %)) s)
      (primes (inc n) s)
      (cons n (primes (inc n) (cons n s))))))

;(println (take 100 (primes 2 [])))

((fn [m] (take m ((fn p [n s]
                    (lazy-seq
                      (if (some #(= 0 (mod n %)) s)
                        (p (inc n) s)
                        (cons n (p (inc n) (cons n s)))))) 2 []))) 10)

; 69
(defn mm [f & ms]
  (reduce
    (fn [acc, map] (reduce (fn [m [k v]] (update-in m [k] (fn [v2] (if v2 (f v2 v) v)))) acc map))
    ms))

;(println (mm - {:a 1 :b 2} {:a 3 :c 4} {:d 100 :a -100}))

;70
(defn so [s]
  (sort-by clojure.string/lower-case (clojure.string/split s #"\W+")))
;(println (so "cc! Xx. aaa Bb-v"))


;73
(defn g [b]
  (concat b (apply map vector b) [[((b 0) 0) ((b 1) 1) ((b 2) 2)] [((b 2) 0) ((b 1) 1) ((b 0) 2)]]))

(def bb [[:x :e :o]
         [:o :x :e]
         [:x :e :x]])


;(println (some #(every? (fn[a] (= a :e)) %) bb))

(defn tictoe [b]
  (let [m (concat b (apply map vector b) [[((b 0) 0) ((b 1) 1) ((b 2) 2)] [((b 2) 0) ((b 1) 1) ((b 0) 2)]])
        w (fn [p] (some #(every? (fn [a] (= a p)) %) m))]
    (cond (w :x) :x (w :o) :o :else nil)))

(defn tictoe2 [b]
  (concat b (apply map vector b) [[((b 0) 0) ((b 1) 1) ((b 2) 2)] [((b 2) 0) ((b 1) 1) ((b 0) 2)]]))

;(println (tictoe bb))

(filter #(let [r (int (Math/sqrt %))] (= (* r r) %)) (map read-string (re-seq #"\d+" "4,5,6,7,8,9")))

((fn [s] (apply str (interpose "," (filter #(let [r (int (Math/sqrt %))] (= (* r r) %)) (map read-string (re-seq #"\d+" s)))))) "4,5,6,7,8,9,10,16")

;75
(defn tot [n]
  (letfn [(g [a b] (if (= b 0) a (recur b (mod a b))))]
    (count (filter (fn [i] (= i 1)) (map (partial g n) (range 1 (inc n)))))))

;(println (tot 10))
;(println (tot 1))
;(println (tot 99))

#(letfn [(g [a b] (if (= b 0) a (recur b (mod a b))))]
  (count (filter (fn [i] (= i 1)) (map (partial g %) (range 1 (inc %))))))

;77
#(set (mapcat (fn [[k v]] (when (> (count v) 1) [(set v)])) (group-by sort %)))

;78
(defn tramp
  ([f & args] (tramp (apply f args)))
  ([f] (if (not (fn? f)) f (recur (f)))))

;(println (letfn [(triple [x] #(sub-two (* 3 x)))
;        (sub-two [x] #(stop?(- x 2)))
;        (stop? [x] (if (> x 50) x #(triple x)))]
;  (tramp triple 2)))

;79
(defn min-path [triangle]
  (letfn [(calc [acc line]
            (vec (map-indexed
                   (fn [i e]
                     (let [pa (delay (acc (dec i))) pb (delay (acc i))]
                       (cond (= 1 (count line)) [e [e]]
                             (= i 0) [(+ e (@pb 0)) (conj (@pb 1) e)]
                             (= (inc i) (count line)) [(+ e (@pa 0)) (conj (@pa 1) e)]
                             :else (if (< (@pa 0) (@pb 0))
                                     [(+ e (@pa 0)) (conj (@pa 1) e)]
                                     [(+ e (@pb 0)) (conj (@pb 1) e)])))) line)))]
    (second (apply min-key #(% 0) (reduce calc [] triangle)))))

(defn min-path2 [triangle]
  (letfn [(calc [acc line]
            (vec (map-indexed
                   (fn [i e]
                     (let [pa (delay (acc (dec i))) pb (delay (acc i))]
                       (cond (= 1 (count line)) e
                             (= i 0) (+ e @pb)
                             (= (inc i) (count line)) (+ e @pa)
                             :else (if (< @pa @pb) (+ e @pa) (+ e @pb))))) line)))]
    (apply min (reduce calc [] triangle))))

;(println (min-path [[1] [2 3] [0 3 5]]))
;(println (min-path2 '([3]
;                       [2 4]
;                       [1 9 3]
;                       [9 9 2 4]
;                       [4 6 6 7 8]
;                       [5 7 3 5 1 4])))


;((#(let [d clojure.set/difference u (clojure.set/union % %2)] (d (d u %)))) #{1 2 3 4} #{3 4 5})

; 82
(defn ds [a b]
  (if (= (first a) (first b)) (ds (rest a) (rest b)) [a b]))

(defn issim [a b]
  (let [[c d] (map count (apply ds (map reverse (ds a b))))]
    (or (= 1 c d) (= 1 (+ c d)))))

(defn mut [i s]
  (map #(let [[a b] (split-at % s)] (concat a [i] b)) (range (inc (count s)))))

(defn perm [s]
  (if (seq s)
    (mapcat (partial mut (first s)) (perm (rest s)))
    [[]]))

(defn iss [s]
  (every? (partial apply issim) (map vector s (rest s))))

(defn alt [s]
  (some iss (perm s)))

;(println (perm [1 2 3]))
;(println (alt #{"cot" "hot" "bat" "fat"}))


; 84

(defn tc-iter [[bs s]]
  (let [n (mapcat (fn [[a b]] (mapcat (fn [[c d]] (when (and (= b c) (not (s [a d]))) [[a d]])) s)) bs)]
    [n (clojure.set/union s n)]))

(defn tc [s]
  (last (last (take-while (comp seq first) (iterate tc-iter [s s])))))

(tc #{[8 4] [9 3] [4 2] [27 9]})

(fn [s]
  (letfn [(i [[bs s]]
            (let [n (mapcat (fn [[a b]] (mapcat (fn [[c d]] (when (and (= b c) (not (s [a d]))) [[a d]])) s)) bs)]
              [n (clojure.set/union s n)]))]
    (concat (take-while seq (map first (iterate i [s s]))))))

(fn xx [s]
  (letfn [(i [[bs s]]
            (let [n (mapcat (fn [[a b]] (mapcat (fn [[c d]] (when (and (= b c) (not (s [a d]))) [[a d]])) s)) bs)]
              [n (clojure.set/union s n)]))]
    (last (last (take-while (comp seq first) (iterate i [s s]))))))

(let [divides #{[8 4] [9 3] [4 2] [27 9]}]
  (= (xx divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))

;85
(defn ps [s]
  (if (seq s)
    (let [r (ps (rest s))]
      (clojure.set/union r (map #(conj % (first s)) r)))
    #{#{}}))

;86

(defn digits [n]
  (map last (take-while #(> (first %) 0) (iterate
                                           (fn [[a b]]
                                             (let [m (/ (- a b) 10)] [m (mod m 10)]))
                                           [n (mod n 10)]))))

(defn hn [n]
  (apply + (map #(* % %) (digits n))))

(defn ish
  ([n] (ish n #{}))
  ([n s] (let [h (hn n)] (cond (= h 1) true (s h) false :else (recur h (conj s h))))))


