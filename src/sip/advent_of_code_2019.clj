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
(defn parse-guard-log [line]
  (let [[year month day hour minute msg id] 
          (rest (first (re-seq #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (.{5}) (#\d+)?" line)))
        [year month day hour minute id] (map #(Integer/parseInt %) [year month day hour minute (if id (subs id 1) "0")])]
    {:year year, :month month, :day day, :hour hour, :minute minute, :id id,
     :tag (condp = msg 
            "falls" :fall
            "wakes" :wake
            "Guard" :begin msg)}))      

(def input4
  (-> "src/sip/adv-input4.txt"
      (slurp)
      (clojure.string/split #"\n")
      (#(mapv parse-guard-log %))))


(defn get-sleep-intervals [guard-log]
  (->> guard-log
      (sort-by (juxt :year :month :day :hour :minute))
      (reduce (fn [[cid v] record]
                (let [prev (peek v)]
                  (condp = (:tag record)
                    :begin [(:id record) v]
                    :fall [cid (conj v {:id cid, :start (:minute record), :end 59})]
                    :wake [cid (assoc v 
                                      (dec (count v))
                                      (assoc prev :end (:minute record)))])))
              [-1 []])
      (second)))
     
(defn adv7 []
  (let [intervals (get-sleep-intervals input4)
        most-sleeping 
          (->> intervals
               (group-by :id)
               (map (fn [[id records]] 
                      {:id id
                       :total (reduce (fn [acc rec] (+ acc (- (:end rec) (:start rec)))) 0 records)}))
               (apply max-key :total)
               (:id))]
    (->> intervals
         (filter #(= (:id %) most-sleeping))
         (mapcat #(range (:start %) (:end %)))
         (frequencies)
         (apply max-key val)
         (key)
         (* most-sleeping))))
   
(defn adv8[]
  (let [intervals (get-sleep-intervals input4)]
    (->> intervals 
         (mapcat (fn [rec] 
                   (map (fn [minute] 
                          {:id (:id rec), :minute minute})  
                        (range (:start rec) (:end rec)))))
         (frequencies)
         (apply max-key val)
         (key)
         ((fn [{:keys [id minute]}] (* id minute))))))

; DAY 5


(def input5
  (->> "src/sip/adv-input5.txt"
       (slurp)
       (clojure.string/trim-newline)))

(defn react? [a b] 
  (= 32 (Math/abs (- (int a) (int b)))))

(defn adv9 
  ([] (adv9 [] input5))
  ([a b] (cond 
           (empty? b) a
           (and (seq a) (react? (peek a) (first b))) 
             (recur (subvec a 0 (dec (count a))) (rest b))
           :else (recur (conj a (first b)) (rest b)))))   

(defn replace-i [s i]
  (clojure.string/replace 
    s 
    (re-pattern (str \[ (char (+ i 65)) (char (+ 32 65 i)) \]))
    ""))

(defn adv10 []        
  (->> (range 0 26)
       (map #(replace-i input5 %))
       (map (comp count #(adv9 [] %)))
       (sort)
       (first)))

;# DAY 6

(def input6
  (-> "src/sip/adv-input6.txt"
      (slurp)
      (clojure.string/split #"\n")
      (->> 
          (mapv (fn [line] (re-seq #"\d+" line)))
          (mapv (fn [points] (map #(Integer/parseInt %) points))))))

(defn get-limits [points]
  (reduce (fn [{:keys [minx maxx miny maxy]} [x y]]
            {:minx (if (< x minx) x minx)
             :maxx (if (> x maxx) x maxx)
             :miny (if (< y miny) y miny)
             :maxy (if (> y maxy) y maxy)})
           {:minx Integer/MAX_VALUE, :maxx 0, :miny Integer/MAX_VALUE, :maxy 0}
           points))

(defn get-min-with-ties [positive-ints] 
  (->> positive-ints
       (map vector (range))
       (reduce (fn [[imin indexes] [index elem]]
                 (cond
                   (< elem imin) [elem [index]]
                   (= elem imin) [elem (conj indexes index)]
                   :else [imin indexes]))
               [Integer/MAX_VALUE []])))

(defn get-closest-index [[xa ya] points]
  (->> points 
       (map (fn [[x y]] (+ (Math/abs (- xa x)) (Math/abs (- ya y)))))
       (get-min-with-ties)
       ((fn [[imin indexes]] (if (= (count indexes) 1) (first indexes) -1))))) 

(defn get-closest-map [points limits]
  (for [x (range (:minx limits) (inc (:maxx limits)))
        y (range (:miny limits) (inc (:maxy limits)))]
    {:x x, :y y, :ci (get-closest-index [x y] points)}))    

(defn get-ignorables [closest limits]
  (->> closest
      (filter 
        (fn [{:keys [x y ci]}] 
          (or (= x (:minx limits)) (= x (:xmax limits))
              (= y (:miny limits)) (= y (:maxy limits)))))
      (map :ci) 
      (set)
      (#(conj % -1))))

(defn adv11
  ([] (adv11 input6))
  ([points] 
    (let [limits (get-limits points)
          closest (get-closest-map points limits)
          ignorables (get-ignorables closest limits)]
      (->> closest
           (filter #(not (ignorables (:ci %)))) 
           (map :ci)
           (frequencies)
           (apply max-key second)
           (second)))))
  
  
