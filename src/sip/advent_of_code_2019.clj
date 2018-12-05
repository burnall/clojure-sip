(ns sip.advent-of-code-2019)

; https://adventofcode.com/2018

; DAY 1
(def input1
  (-> "src/sip/adv-input1.txt"
      (slurp)
      (clojure.string/split #"\n")
      ((fn [s] (map #(Integer/parseInt %) s)))))
 

(defn adv1 [] 
  (reduce + input1))

(defn find-non-unique [occur [x & xs]]
  (if (occur x)
    (lazy-seq (cons x (find-non-unique occur xs)))
    (recur (conj occur x) xs))) 
  

(defn adv2 
  ([] (adv2 input1))  
  ([s]
    (->> s
         (cycle)
         (reductions + 0)
         (find-non-unique #{})
         (take 10))))

; DAY 2

(def input2
  (-> "src/sip/adv-input2.txt"
      (slurp)
      (clojure.string/split #"\n")))

(defn get-checksum-values [line]
  (letfn [(has-value [m v] (some (partial = v) (vals m)))
          (f [b] (if b 1 0))] 
    (-> line
        (frequencies)
        ((fn [m] [(f (has-value m 2)) 
                  (f (has-value m 3))])))))
     
(defn adv3 [] 
  (->> input2
       (map get-checksum-values)
       ((fn [checksums] (* (reduce + (map first checksums)) (reduce + (map second checksums))))))) 

(defn diff-lists[xa xb] 
  (reduce (fn [acc [a b]] 
            (if (= a b) 
              acc
              (if (zero? acc) 1 (reduced 2)))) 
          0
          (map vector xa xb)))

(defn find-diff-1 []
  (for [i (range (count input2))
        j (range i (count input2))
        :when (= 1 (diff-lists (input2 i) (input2 j)))]
    [i j]))

(defn extract-similarity [xa xb]
  (reduce (fn [acc [a b]] (if (= a b) (conj acc a) acc))
          []
          (map vector xa xb)))

(defn adv4[] 
  (->> (find-diff-1)
       (first)
       (map input2)
       (apply extract-similarity)
       (apply str))) 

; DAY 3

; Line sample "#1 @ 265,241: 16x26"
(defn parse-square [line]
  (->> line
       (re-seq #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
       (first)
       (next)
       (map #(Integer/parseInt %))
       ((fn [[id x0 y0 dx dy]] {:id id, :a [x0 y0], :b [(+ x0 dx) (+ y0 dy)]}))))

(def input3
  (-> "src/sip/adv-input3.txt"
      (slurp)
      (clojure.string/split #"\n")
      (#(mapv parse-square %))))


(defn intersect-1-dim [a0 a1 b0 b1]
  (cond 
    (and (>= a0 b0) (< a0 b1)) [a0 (min a1 b1)]
    (and (>= b0 a0) (< b0 a1)) [b0 (min a1 b1)]))

(defn intersect-2-dim [[xa0 ya0] [xa1 ya1] [xb0 yb0] [xb1 yb1]]
  (let [[x0 x1] (intersect-1-dim xa0 xa1 xb0 xb1)
        [y0 y1] (intersect-1-dim ya0 ya1 yb0 yb1)]
    (when (and x0 y0) 
          [[x0 y0] [x1 y1]])))

(defn make-couples [len]
  (for [i (range len)
        j (range (inc i) len)]
    [i j]))    
      
(defn scatter-square [[x0 y0] [x1 y1]]
  (for [i (range x0 x1)
        j (range y0 y1)]
    [i j]))

(defn compare-elements [i j]
  (let [{a0 :a, a1 :b} (input3 i)
        {b0 :a, b1 :b} (input3 j)]
    (intersect-2-dim a0 a1 b0 b1))) 

(defn adv5[]
  (->> (count input3)
       (make-couples)
       (map (partial apply compare-elements))
       (filter some?)
       (mapcat (partial apply scatter-square))
       (distinct)
       (count)))

(defn has-no-intersections [i]
  (every? (fn [[el j]] (or (= i j) (nil? (compare-elements i j))))
          (map vector input3 (range))))

(defn adv6[] 
  (->> (count input3)
       (range)
       (filter has-no-intersections)
       (map input3)))

; DAY 4

; [1518-10-23 23:47] Guard #1627 begins shift

