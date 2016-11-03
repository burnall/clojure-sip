(ns puz3)

(defn it [& colls]
  (let [step
        (fn step [cs]
          (lazy-seq
            (let [ss (map seq cs)]
              (when (some identity ss)
                (cons (map #(if % (first %) nil) ss) (step (map rest ss)))))))]
    (step colls)))

(defn f [s] (reduce (fn [[mi me] [i e]] (if (< e me) [i e] [mi me])) (map-indexed vector s)))




(defn si [& colls]
  (let [mi (fn [s] (first (reduce (fn [[mi me] [i e]] (if (< e me) [i e] [mi me])) (map-indexed list s))))]
    (cond
      (some empty? colls) nil
      (apply = (map first colls)) (first (first colls))
      :else (recur (update-in (vec colls) [(mi (map first colls))] rest)))))

(println (si [1 2 4 5]))

;110

(defn f [s]
  (mapcat #(-> [(count %) (first %)]) (partition-by identity s)))


(defn ps [v]
  (rest (iterate (fn [s] (vec (mapcat #(-> [(count %) (first %)]) (partition-by identity s)))) v)))

;111

(defn cw? [w m]
  (let [p (map (fn [s] (clojure.string/replace s #"\s|_" #({" " "" "_" "."} %1))) m)
        fl (concat p (apply map (comp (partial apply str) list) p))
        ws (mapcat #(clojure.string/split % #"[#]+") fl)]
    (some? (some #(re-find (re-pattern (str "^" % "$")) w) ws))))


(cw? "ops"
     ["c o n j"
      "_ _ y _"
      "r _ _ #"])

;112



(defn sf [n s]
  (mapcat (fn [i] (if (coll? i) [(sf n i)] [i])) s))

(defn sf [n s]
  (let [[a & r] s]
    (cond
      (nil? a) [n []]
      (coll? a)
      (let [[i t] (sf n a) [j u] (sf i r)]
        [j (concat [t] u)])
      (> a n)
      (let [[i t] (sf (- n a) r)]
        [i t])
      :else
      (let [[i t] (sf (- n a) r)]
        [i (concat [a] t)]))))

#(last ((fn f [n s]
          (let [[a & r] s]
            (cond
              (nil? a) [n []]
              (coll? a)
              (let [[i t] (f n a) [j u] (f i r)]
                [j (concat [t] u)])
              (> a n)
              [-1 []]
              :else
              (let [[i t] (f (- n a) r)]
                [i (concat [a] t)])))) % %2))




(sf 10 [1 2 [3 [4 5] 6] 7])

(str (let [f "foo"]
       (reify Object
         (toString [this] f))))

(seq (let [f "foo"]
       (reify clojure.lang.Seqable
         (seq [this] (seq f)))))

(defn f [& s]
  (reify
    Object (toString [this] (apply str (interpose ", " (sort s))))
    clojure.lang.Seqable (seq [this] (distinct s))))

;114

(defn tw [n p s]
  (lazy-seq
    (let [[a & b] s]
      (when a
        (if (p a)
          (when (> n 1) (cons a (tw (dec n) p b)))
          (cons a (tw n p b)))))))

(tw 4 #(= 2 (mod % 3)) [2 3 5 7 11 13 17 19 23])


; 115
(defn s? [s]
  (let [t (map (comp bigint str) (str s)) c (/ (count t) 2)]
    (= (apply + (take c t)) (apply + (take-last c t)))))

; 116

(defn p? [n] (every? #(> (mod n %) 0) (range 2 n)))
(defn i? [i]
  (let [a (p? (- 563 i)) b (p? (+ 563 i))]
    (cond
      (= true a b) true
      (= a b) (recur (inc i))
      :else false)))

(defn bp? [n]
  (letfn [(p? [i] (every? #(> (mod i %) 0) (range 2 i)))
          (i? [i]
            (let [a (p? (- n i)) b (p? (+ n i))]
              (cond
                (= true a b) true
                (= a b) (recur (inc i))
                :else false)))]
    (and (> n 2) (p? n) (i? 1))))

; 117

(defn m? [m]
  (let
    [v (vec (map vec m))
     a (count m)
     b (count (m 0))
     e? (fn [i j] (when (and (> i -1) (< i a) (> j -1) (< j b) (not= ((v i) j) \#)) [i j]))
     f (fn [c] (first (for [[i r] (map-indexed vector v)
                            [j e] (map-indexed vector r)
                            :when (= e c)] [i j])))
     pe (f \C)
     nn (fn [[i j]] (filter some? [(e? (dec i) j) (e? i (dec j)) (e? i (inc j)) (e? (inc i) j)]))
     g (fn [s]
         (let [t (reduce #(apply conj % (nn %2)) s s)]
           (cond
             (t pe) true
             (= (count t) (count s)) false
             :else (recur t))))]
    (g #{(f \M)})))



(m? ["#######"
     "#     #"
     "#  #  #"
     "#M # C#"
     "#######"])

;118
(fn g [f s] (lazy-seq (when (seq s) (cons (f (first s)) (g f (rest s))))))


; 119

(defn wm [p m]
  (let
    [w? (fn [b] (let [a (concat b (apply map vector b) [[((b 0) 0) ((b 1) 1) ((b 2) 2)] [((b 2) 0) ((b 1) 1) ((b 0) 2)]])]
                  (some #(every? (fn [a] (= a p)) %) a)))]
    (for [[i r] (map-indexed vector m)
          [j e] (map-indexed vector r)
          :when (and (= :e e) (w? (assoc-in m [i j] p)))]
      [i j])))

(defn wm [p m]
  (let
    [w? (fn [b] (let [a (concat b (apply map vector b) [[((b 0) 0) ((b 1) 1) ((b 2) 2)] [((b 2) 0) ((b 1) 1) ((b 0) 2)]])]
                  (some #(every? (fn [a] (= a p)) %) a)))]
    (w? m)))


(wm :x [[:o :e :e] [:o :x :o] [:x :x :e]])

(wm :x [[:o :e :e]
        [:o :x :o]
        [:x :x :e]])

(defn xx[b]
  (let [m (concat b (apply map vector b) [[((b 0) 0) ((b 1) 1) ((b 2) 2)] [((b 2) 0) ((b 1) 1) ((b 0) 2)]])
        w (fn [p] (some #(every? (fn [a] (= a p)) %) m))]
    (cond (w :x) :x (w :o) :o :else nil)))

;121

(defn f [s]
  (fn [m]
    (let
      [op {'+ + '- - '/ / '* *}
       g (fn g [t]
           (apply (op (first t)) (map #(cond (coll? %) (g %) (symbol? %) (m %) :else %) (rest t))))]
      (g s))))

;124

(defn r? [m c]
  (let
    [op (if (= c 'b) 'w 'b)
    g (fn[i j] (if (and (> i -1) (< i (count m)) (> j -1) (< j (count (m 0)))) ((m i) j) 0))
    mg (fn mg[i j di dj s]
          (let [ni (+ i di) nj (+ j dj) nc (g ni nj)]
            (cond (= nc op) (mg ni nj di dj (conj s [ni nj]))
                  (and (= nc 'e) (seq s)) [[ni nj] s])))
    ms (fn [i j e] (when (= e c) (mapcat (fn [[di dj]] (mg i j di dj #{})) [[-1 1] [-1 0] [-1 -1] [0 -1] [1 -1] [1 0] [1 1] [0 1]])))
    s (for [[i r] (map-indexed vector m)
            [j e] (map-indexed vector r)
            :let [s (ms i j e)]
            :when (seq s)]
        (ms i j e))]
    (apply hash-map (mapcat identity s))))


  ;(= {[0 3] #{[2 1] [1 2]}, [1 3] #{[1 2]}, [2 3] #{[2 1] [2 2]}}

  (r? '[[e e w e]
        [b b w e]
        [b w w e]
        [b w w w]] 'b)

  (r? '[[e e w e] [b b w e] [b w w e] [b w w w]] 'b)

;125

(let [a "(let\n"] a)

(#(list % (list 'quote %)) '#(list % (list 'quote %)))

(#(list % (list 'quote %)) '#(list (fn[] (str % (list 'quote %)))))


((fn [] (fn [a] (list a (list (quote quote) a)))) (quote (fn [] (fn [a] (list a (list (quote quote) a))))))

(fn [] (str ((fn [a] (list a (list (quote quote) a))) (quote (fn [] (str (fn [a] (list a (list (quote quote) a)))))))))
(fn [] (str ((fn [a] (list a (list (quote quote) a))) (quote (fn [] (str (fn [a] (list a (list (quote quote) a)))))))))


(fn [] (let [a ["(fn [] (let [a " "] (apply str (a 0) a (a 1))))"]] (apply str (a 0) a (a 1))))

;128
(defn ff[[s c]] {:suit ({\S :spade \H :heart \D :diamond \C :club} s) :rank (or ({\T 8 \J 9 \Q 10 \K 11 \A 12} c) (- (int c) 50))})

