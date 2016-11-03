(ns puz5)
(require 'clojure.set)

(defn f [v] (iterate (fn [w] (vec (concat [(w 0)] (map +' w (rest w)) [(last w)]))) v))

;(println (take 2 (f [3 1 2])))

;153
(#(= (count (apply concat %)) (count (apply clojure.set/union %))) #{#{:d} #{:b :c :a}})

;144 Oscilrate
(defn oscilrate [v & f]
  (iterate (fn [a [x & xs]] [(x a) xs])
           [v (cycle f)]))

(defn oscilrate [v & f] (map first (iterate (fn [[a [x & xs]]] [(x a) xs]) [v (cycle f)])))

(fn [v & f] (map first (iterate (fn [[a [x & xs]]] [(x a) xs]) [v (cycle f)])))

;158 Decurry

(defn decurry [f]
  (fn [s] (reduce #(% %2) f s)))


(defn decurry2 [f] (fn [& s] (reduce #(% %2) f s)))
(fn [f] (fn [& s] (reduce #(% %2) f s)))


; 148 Big Divide
(defn divn [n a b]
  (let [f (fn [x] (let [g (bigint (/ n x))] (/ (* g (inc g) x) 2)))]
    (+ (f a) (f b) (- (f (* a b))))))

; 171 Intervals
(defn ints [z]
  (let [[v w] (reduce (fn [[[a b] t] i]
                        (cond (nil? a) [[i i] []]
                              (= i (inc b)) [[a i] t]
                              1 [[i i] (conj t [a b])]))
                      [[] []]
                      (sort z))] (conj w v)))

;[10 9 8 1 2 3]
; 177 Balancing Brackets
(defn bb [s]
  (empty? (reduce (fn [t c] (cond
                              (some #(= c %) "({[") (conj t c)
                              (= (peek t) ({\) \( \} \{ \] \[} c)) (pop t)
                              (some #(= c %) ")}]") (reduced [1])
                              1 t)) [] s)))

(fn [s]
  (empty? (reduce (fn [t c] (cond
                              (some #(= c %) "({[") (conj t c)
                              (and (seq t) (= (peek t) ({\) \( \} \{ \] \[} c))) (pop t)
                              (some #(= c %) ")}]") [1]
                              1 t)) [] s)))

; 141 Tricky card games
(defn winner [t]
  (fn [s] (reduce #(cond (= (% :suit) (%2 :suit)) (if (> (%2 :rank) (% :rank)) %2 %)
                         (= t (%2 :suit)) %2
                         1 %)
                  s)))

; 150 Palindromic Numbers

(defn next [a]
  (cond (< a 9) (inc a)
        (every? #(= \9 %) (str a)) (next (inc a))
        1 (let [s (str a)
                l (count s)
                h (int (/ l 2))
                p1 (subs s 0 h)
                p2 (subs s (- l h))
                md (subs s h (- l h))
                f (fn [s] (Long/parseLong s))
                r1 (apply str (reverse p1))
                mk (fn [a b] (f (apply str (concat a b (reverse a)))))]

            (cond (> (f r1) (f p2)) (mk p1 md)
                  (seq md) (if (= md "9")
                             (mk (str (inc (f p1))) [\0])
                             (mk p1 [(char (inc (int (first md))))]))
                  1 (mk (str (inc (f p1))) md)))))



; 168 Infinite Matrix
(defn gm
  ([f] (let [
             g (fn g [i] (lazy-seq (cons i (g (inc i)))))
             h (fn h [i j] (lazy-seq (cons (f i j) (h i (inc j)))))
             ] (map #(h % 0) (g 0))))
  ([f m n] (let
             [g (fn [s i] (if (= i 0) s (recur (rest s) (dec i))))
              ] (map #(g % n) (g (gm f) m))))
  ([f m n s t] (let
                 [g (fn g [[a & b] i] (if (= i 0) [] (cons a (g b (dec i)))))
                  ] (map #(g % t) (g (gm f m n) s)))))

; 195 Parentheses... Again
(defn gen [n]
  (set (map (comp (partial apply str) last) (loop [i (* 2 n) s [[n 0 []]]]
                                              (if (= i 0)
                                                s
                                                (recur (dec i) (mapcat (fn [[a d t]]
                                                                         (cond (= d 0) [[(dec a) (inc d) (conj t \()]]
                                                                               (= a 0) [[a (dec d) (conj t \))]]
                                                                               1 [[(dec a) (inc d) (conj t \()] [a (dec d) (conj t \))]]))
                                                                       s)))))))

; 178 Best Hand
;Straight flush: All cards in the same suit, and in sequence
;Four of a kind: Four of the cards have the same rank
;Full House: Three cards of one rank, the other two of another rank
;Flush: All cards in the same suit
;Straight: All cards in sequence (aces can be high or low, but not both at once)
;Three of a kind: Three of the cards have the same rank
;Two pair: Two pairs of cards have the same rank
;Pair: Two cards have the same rank
;High card: None of the above conditions are met

;(= :high-card (__ ["HA" "D2" "H3" "C9" "DJ"]))
;(bh ["HA" "D2" "H3" "C9" "DJ"])

;(fn[[s c]] {:suit ({\S :spade \H :heart \D :diamond \C :club} s) :rank (or ({\T 8 \J 9 \Q 10 \K 11 \A 12} c) (- (int c) 50))})

(defn bh [s]
  (let [f (fn [[a b]] [a (or ({\T 8 \J 9 \Q 10 \K 11 \A 12} b) (- (int b) 50))])
        t (map f s)
        f? (apply = (map first t))
        sl (sort (map second t))
        s? (apply = (conj (map #(= (inc %) %2) sl (rest sl)) true))
        [m n] (take 2 (reverse (sort (map count (partition-by identity sl)))))
        ]
    (if f?
      (if s? :straight-flush :flush)
      (cond
        (or s? (= sl [0 1 2 3 12])) :straight
        (= 4 m) :four-of-a-kind
        (and (= 3 m) (= n 2)) :full-house
        (= 3 m) :three-of-a-kind
        (and (= 2 2) (= n 2)) :two-pair
        (= 2 m) :pair
        1 :high-card))))


; 130 Tree reparenting
;(tp 'a '(t (e) (a)))
(def x #(let
         [f (fn f [[v l r]]
              (cond (= % v) (cond r (list [v l r]) l (list [v l]) 1 (list [v]))
                    (nil? l) nil
                    1 (if-let [a (f l)] (conj a (if r [v r] [v]))
                                        (if-let [b (f r)] (conj b (if l [v l] [v])))
                                        )))]
         (reduce (fn [a b] (conj b a)) (f %2))))

(x 'c '(a
         (b
           (c
             (d)
             (e))
           (f
             (g)
             (h)))
         (i
           (j
             (k)
             (l))
           (m
             (n)
             (o)))))

(x 'a '(t (e) (a)))
(x 'e '(a (t (e))))

#(letfn
  [(g [a [v & c]] (when v (if-let [b (f v)] (conj b (concat a c)) (recur (concat a [v]) c))))

   (f [[s & t]]
     (cond (= % s) (list (concat (list s) t))
           (not t) nil
           1 (g [s] t)))]
  (reduce (fn [a b] (concat b [a])) (f %2)))



(def x #(letfn
  [(g [a [v & c]] (do (println a v c) (when v (if-let [b (f v)] (concat b [(concat a c)]) (recur (concat a [v]) c)))))

   (f [[s & t]] (do (println 333 s t)
              (cond (= % s) (concat [s] t)
                    (not t) nil
                    1 (g [s] t))))]
  (f %2)))