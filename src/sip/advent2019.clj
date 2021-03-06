(ns sip.advent2019)

; https://adventofcode.com/2018

; DAY 1
(def input1
  (-> "src/sip/data/adv-input1.txt"
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
  (-> "src/sip/data/adv-input2.txt"
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
  (-> "src/sip/data/adv-input3.txt"
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
  (-> "src/sip/data/adv-input4.txt"
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
  (->> "src/sip/data/adv-input5.txt"
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
  (-> "src/sip/data/adv-input6.txt"
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

(defn manhattan-distance[[xa ya] [xb yb]]
  (+ (Math/abs (- xa xb)) (Math/abs (- ya yb))))

(defn get-closest-index [[xa ya] points]
  (->> points 
       (map (partial manhattan-distance [xa ya]))
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

(defn sum-distance [points [xa ya]]
  (reduce (fn [acc [x y]] (+ acc (manhattan-distance [xa ya] [x y])))
          0
          points))

(defn get-points-inside-limits [points limits]
  (for [x (range (:minx limits) (inc (:maxx limits)))
        y (range (:miny limits) (inc (:maxy limits)))]
    [x y]))    


(defn adv12 
  ([] (adv12 input6))
  ([points]
    (let [limits (get-limits points)
          points-inside (get-points-inside-limits points limits)] 
      (->> points-inside
           (map (partial sum-distance points)) 
           (filter (partial > 10000))
           (count)))))

; DAY 7

;Step G must be finished before step L can begin.

(defn get-deps [data]
  (->> data
       (reduce (fn [{:keys [before-tasks after-tasks]} [fst snd]]
                 (let [before-tasks 
                        (if (before-tasks fst) before-tasks (assoc before-tasks fst #{}))] 
                   {:before-tasks (update before-tasks snd (fn [value] (conj (or value #{}) fst)))
                    :after-tasks (update after-tasks fst (fn [value] (conj (or value #{}) snd)))}))
               {:before-tasks {}, :after-tasks {}})))

(def input7
  (-> "src/sip/data/adv-input7.txt"
      (slurp)
      (clojure.string/split #"\n")
      (->> 
          (mapv (fn [line] (re-seq #"Step (.{1}) must be finished before step (.{1}) can begin." line)))
          (mapv (comp (partial map first) rest first))
          (get-deps))))

(defn next-tasks [before-tasks] 
  (->> before-tasks
       (filter (fn [[task before-t]] (empty? before-t)))
       (map first)))

(defn next-task [before-tasks]
  (->> before-tasks
       (next-tasks)
       (apply min-key int)))

(defn next-tasks-sorted [before-tasks]
  (sort (next-tasks before-tasks)))

(defn drop-task [before-tasks after-tasks task]
  (->> task
       (after-tasks)
       (reduce (fn [tasks unblocked] 
                 (assoc tasks unblocked (disj (tasks unblocked) task)))
               before-tasks)
       ((fn [new-before-tasks] (dissoc new-before-tasks task)))))         

(defn adv13 
  ([] (adv13 input7 []))
  ([{:keys [before-tasks, after-tasks]} solution]
    (if (empty? before-tasks)
      (apply str solution)
      (let [nxt (next-task before-tasks)
            new-before-tasks (drop-task before-tasks after-tasks nxt)]   
         (recur {:before-tasks new-before-tasks, :after-tasks after-tasks} (conj solution nxt))))))


(defn workload-update [workload]
  (let [min-time (:time-left (apply min-key :time-left workload))
        finished (filter #(= (:time-left %) min-time) workload)
        pending (filter #(> (:time-left %) min-time) workload)
        pending-updated (map #(update % :time-left (fn [time] (- time min-time))) pending)]
    {:finished (map :task finished)
     :pending pending-updated
     :delta min-time}))

(defn next-tasks-without-pending [before-tasks tasks-count pending-tasks]
  (->> before-tasks
       (next-tasks-sorted)
       (filter (fn [task] (every? #(not= task %) pending-tasks)))
       (take tasks-count)))
  
(defn drop-tasks [before-tasks after-tasks tasks]
  ;(prn "drop-tasks" before-tasks after-tasks tasks)
  (reduce (fn [new-before-tasks task]
            (drop-task new-before-tasks after-tasks task))
          before-tasks
          tasks))

(defn get-task-duration [task]
  (+ 61 (- (int task) (int \A))))


(defn adv14 
  ([] (adv14 input7 5))
  ([{:keys [before-tasks after-tasks]} workers-count]
    (let [
      make-workload (fn [task] {:task task, :time-left (get-task-duration task)})
      initial-workload (
        ->> (next-tasks-sorted before-tasks)
            (take workers-count)
            (map make-workload))
      
      impl (fn [before-tasks workload cur-time]
        ;(prn "impl")
        (let [{:keys [finished pending delta]} (workload-update workload)
              new-before-tasks (drop-tasks before-tasks after-tasks finished)
              new-time (+ cur-time delta)]
          ;(prn (Thread/sleep 500) before-tasks workload finished)    
          (if (empty? new-before-tasks)
             new-time
             (recur new-before-tasks
                    (->> pending 
                         (map :task)
                         (next-tasks-without-pending new-before-tasks (- workers-count (count pending)))
                         (map make-workload)
                         (concat pending))
                    new-time))))]
     (impl before-tasks initial-workload 0)))) 

; DAY 8

(def input8
  (-> "src/sip/data/adv-input8.txt"
      (slurp)
      (clojure.string/trim-newline)
      (clojure.string/split #" ")
      (->> 
          (map #(Integer/parseInt %)))))

(defn read-node [data]
  (let [
    [child-count meta-count & tail] data
    [data-after-children-read agg-with-children] 
      (reduce (fn [[data agg] _] 
                (let [[d v] (read-node data)] [d (+ v agg)]))
              [tail 0]
              (range child-count))
    [data-after-meta-read value-with-meta] 
      (reduce (fn [[[meta-i & tail] agg] _] [tail (+ agg meta-i)])
              [data-after-children-read agg-with-children]
              (range meta-count))]
    [data-after-meta-read value-with-meta]))          

(defn adv15 [] (read-node input8))

(defn read-node-b [data]
  (let [[child-count meta-count & tail] data]
    (if (zero? child-count)
      [(drop meta-count tail) (reduce + (take meta-count tail))]
      (let [[data-after-children-read values]
        (->> (range child-count) 
             (reduce (fn [[data values] _] 
                       (let [[d v] (read-node-b data)] [d (conj values v)])) 
                     [tail []]) 
                     (vec))
        value (->> (take meta-count data-after-children-read)
                   (map #(if (<= % (count values)) (values (dec %)) 0))
                   (reduce +))] 
        [(drop meta-count data-after-children-read) value]))))

(defn adv16 [] (read-node-b input8))

; DAY 9

; 452 players; last marble is worth 71250 points

(defn vector-insert [v index elem]
  (vec (concat (subvec v 0 index) 
               [elem]
               (if (> (count v) index) (subvec v index) []))))   

(defn vector-delete [v index]
  (vec (concat (subvec v 0 index) (subvec v (inc index)))))

(defn next-position [[winnings position current marble-no]]
  (let [marble-no (inc marble-no)]
  (if (zero? (mod marble-no 23))
    (let [current (mod (- current 7) (count position))
          player-no (mod marble-no (count winnings))
          raise (+ marble-no (position current))
          winnings (update winnings player-no #(+ % raise))
          position (vector-delete position current)
          current (if (= current (count position)) 0 current)]
      [winnings position current marble-no])
    (let [current (inc (mod (inc current) (count position)))
          position (vector-insert position current marble-no)]
      [winnings position current marble-no]))))    

(defn gtest [player-count numb]
  (-> [(vec (repeat player-count 0)) [0] 0 0]
      (->> (iterate next-position)
           (take numb))))
    

(defn adv17 [player-count move-count]
  (->> [(vec (repeat player-count 0)) [0] 0 0]
       (iterate next-position)
       (#(nth % move-count))
       (first)
       (apply max)))

(defn adv18 [] )

; DAY 10

(def input10
  (-> "src/sip/data/adv-input10.txt"
      (slurp)
      (clojure.string/split #"\n")
      (->> 
          (map (partial re-seq #"-?\d+"))
          (map (partial map #(Integer/parseInt %)))
          (map (fn [[a b c d]] {:position [a b], :velocity [c d]})))))

(defn get-std [positions]
  (let [
    cnt (count positions)
    average-x (/ (reduce + (map first positions)) cnt)
    average-y (/ (reduce + (map second positions)) cnt)
    sqr-x (reduce (fn [acc x] (Math/pow (- x average-x) 2)) (map first positions))
    sqr-y (reduce (fn [acc y] (Math/pow (- y average-y) 2)) (map second positions))]
    
    (Math/round (/ (Math/sqrt (+ sqr-x sqr-y)) cnt))))

(defn get-same-x-max [positions] 
  (->> positions 
       (map first)
       (frequencies)
       (apply max-key val)
       (val)))

(defn next-sky [stars] 
  (map (fn [{:keys [position velocity]}]
         (let [[px py] position
               [vx vy] velocity]
           {:position [(+ px vx) (+ py vy)]
            :velocity [vx vy]}))
       stars))

(defn star-walk [a b]
  (->> input10
      (iterate next-sky)
      (drop a)
      (take b)
      (mapv (partial map :position))
      ;(mapv get-same-x-max)))
      (mapv get-std)
      (mapv (fn [i std] [std i]) (range))))

(defn star-draw [positions] 
  (let [[minx maxx miny maxy] 
    (->> positions 
         (apply (juxt (partial min-key first) (partial max-key first) (partial min-key second) (partial max-key second)))
         (map #(% %2) [first first second second]))
    field (mapv (fn [_] (vec (repeat (- maxx minx -1) \.))) 
                (range (- maxy miny -1)))
    drawing (reduce (fn [field [x y]] (assoc-in field [(- y miny) (- x minx)] \#))
                    field
                    positions)]
    (->> drawing
         (map (partial apply str))
         (interpose "\n") 
         (apply str))))

(defn draw-loop [start] 
  (let [sky (loop [i start sky (next-sky input10)] (if (> i 0) (recur (dec i) (doall (next-sky sky))) sky)) 
        f (fn [sky start]  
            (prn "seconds" start)
            (print (star-draw (map :position sky)))
            (flush)
            (if-not (= "x" (read-line))
              (recur (next-sky sky) (inc start))))]
    (f sky start)))        
     
; DAY 11

(defn get-power-level [x y serial]
  (let [rack-id (+ 10 x)]
    (-> rack-id 
        (* y)
        (+ serial)
        (* rack-id)
        (mod 1000)
        (quot 100)
        (- 5))))

(defn sum-square [matrix side x-left y-top]
  (->>
       (for [x (range x-left (+ x-left side))
             y (range y-top (+ y-top side))]
          (get-in matrix [y x])) 
       (reduce +)))

(defn adv21 
  ([] (adv21 3 300 1723))
  ([side grid-count serial]
    (let [cells-power (mapv (fn [y] (mapv (fn [x] (get-power-level (inc x) (inc y) serial)) 
                                    (range grid-count)))
                            (range grid-count))]
      (->> (for [x (range (- grid-count side)) 
                 y (range (- grid-count side))] 
             [x y])
           (reduce (fn [[maxv p] [x y]] 
                     (let [curr (sum-square cells-power side x y)]
                       (if (> curr maxv) 
                         [curr [(inc x) (inc y)]]
                         [maxv p])))
                   [-100])))))      

; DAY 12

(defn get-plants-config [lines]
  (let [initial (re-find #"[.#]+" (first lines))]
    (->> lines
         (drop 2 )
         (map (partial re-seq #"([.#]+) => (.)"))
         (map first)
         (mapv (fn [[_ from [to]]] {:from (vec from), :to to}))
         ((fn [rules] {:initial initial, :rules rules})))))

(def input12
  (-> "src/sip/data/adv-input12.txt"
      (slurp)
      (clojure.string/split #"\n")
      (get-plants-config)))

(defn next-plants [plants rules rule-len] 
    (let [
      max-right (- (count plants) rule-len)
      middle (quot rule-len 2)
      iter (fn [current-pos new-plants] 
        (if (= current-pos max-right)  
          new-plants
          (if-let [rule (->> rules
                             (filter (fn [rule] (= (:from rule) (subvec plants current-pos (+ current-pos rule-len)))))
                             (first))]
            (recur (inc current-pos) (assoc new-plants (+ current-pos middle) (:to rule)))  
            (recur (inc current-pos) new-plants))))]
     (vec (iter 0 (vec (repeat (count plants) \.))))))


(defn adv23 
  ([] (adv23 input12 100 5))
  ([plants-config generations rule-len] 
    (let [{:keys [initial rules]} input12
          buf-len (* 2 generations)
          plants (vec (concat (repeat buf-len \.) 
                      initial 
                      (repeat buf-len \.)))
          new-plants  
            (loop [i 0 i-plants plants]
              (prn (clojure.string/trim (clojure.string/replace (apply str i-plants) "." " ")) i (- (.indexOf i-plants \#) buf-len))
              (if (= i generations) 
                i-plants
                (recur (inc i) (next-plants i-plants rules rule-len))))]
     (->> new-plants
          (map (fn [i p] (if (= p \#) i 0)) (range (- buf-len) Integer/MAX_VALUE)) 
          (reduce + (long 0))))))


; DAY 13

(defn find-indexes [carts line]
  (->> line
       (keep-indexed (fn [idx value] (if (carts value) [idx value])))))

(defn clear-chart [chart carts]
  (reduce (fn [agg-chart {:keys [pos value]}] 
            (assoc-in agg-chart 
                      (reverse pos) 
                      ({\v \|, \^ \|, \< \-, \> \-} value)))  
          chart
          carts))

(defn get-carts-config [chart]
  (-> (mapcat (fn [y line] 
                (->> line 
                     (find-indexes #{\v \^ \< \>})
                     (map (fn [[x value]] {:pos [x y], :value value, :turn 0}))))
              (range)
              chart)
      (vec)        
      ((fn [carts] {:chart (clear-chart chart carts) :carts carts, :current-index 0}))))

(def input13
  (-> "src/sip/data/adv-input13.txt"
      (slurp)
      (clojure.string/split #"\n")
      (->> (map vec))
      (vec)
      (get-carts-config)))

(defn index-cart-by-pos [carts pos]
  (->> carts 
       (keep-indexed (fn [idx cart] (when (= pos (:pos cart)) idx)))
       (first)))

(defn get-next-straight [{:keys [value pos turn]} map-value]
  (let [next-pos-map {[\v \|] [[0 1] \v], [\v \\] [[1 0] \>], [\v \/] [[-1 0] \<],
                      [\^ \|] [[0 -1] \^], [\^ \\] [[-1 0] \<], [\^ \/] [[1 0] \>]
                      [\> \-] [[1 0] \>], [\> \\] [[0 1] \v], [\> \/] [[0 -1] \^],
                      [\< \-] [[-1 0] \<], [\< \\] [[0 -1] \^], [\< \/] [[0 1] \v]}
       [delta next-value] (next-pos-map [value map-value])
       next-pos (mapv + pos delta)]
    {:next-value next-value, :next-pos next-pos, :next-turn turn}))

(defn get-next-on-turn [{:keys [value pos turn]} map-value] 
  (let [next-turn-map {\v [\> \v \<]
                       \^ [\< \^ \>]
                       \< [\v \< \^]
                       \> [\^ \> \v]}
        gradient {\v [0 1], \^ [0 -1], \< [-1 0], \> [1 0]}                  
        next-value (get-in next-turn-map [value turn])
        next-pos (mapv + pos (gradient next-value))
        next-turn (mod (inc turn) 3)] 
  {:next-value next-value, :next-pos next-pos, :next-turn next-turn}))

(defn get-next [cart map-value] 
  (if (= map-value \+) 
     (get-next-on-turn cart map-value) 
     (get-next-straight cart map-value)))

(defn find-collision [chart carts current-index]
  (let [cart (carts current-index)
        map-value (get-in chart (reverse (:pos cart)))
        {:keys [next-value next-pos next-turn]} (get-next cart map-value)
        get-next-carts (fn [carts next-index] (if (zero? next-index) (vec (sort-by :pos carts)) carts))]
     
    (if (index-cart-by-pos carts next-pos)
      next-pos
      (let [next-index (mod (inc current-index) (count carts))]   
        (recur chart 
               (get-next-carts (assoc carts current-index {:pos next-pos, :value next-value, :turn next-turn}) next-index)
               next-index)))))

(defn find-remaining-car [chart carts current-index]
  (let [{pos :pos, :as cart} (carts current-index)
        map-value (get-in chart (reverse pos))
       {:keys [next-value next-pos next-turn]} (get-next cart map-value)
       get-next-carts (fn [carts next-index] (if (zero? next-index) (vec (sort-by :pos carts)) carts))
       collided-index (index-cart-by-pos carts next-pos)]
    
    (if (some? collided-index)
      (let [need-next-move (and (= (count carts) 3) (or (zero? current-index) (and (= current-index 1) (= collided-index 0))))   
            next-carts (vec (filter (fn [{p :pos}] (and (not= p pos) (not= p next-pos))) carts))
            next-index (if (< collided-index current-index) (dec current-index) current-index)
            next-index (if (>= next-index (count next-carts)) 0 next-index)
            next-carts (get-next-carts next-carts next-index)]
        (condp = (count next-carts)
          0 "damn!"
          1 (if need-next-move
              (let [last-cart (next-carts 0)
                    map-value (get-in chart (reverse (:pos last-cart)))
                    {last-pos :next-pos} (get-next last-cart map-value)]
                last-pos)    
              (get-in next-carts [0 :pos]))

          (recur chart next-carts next-index)))
      (let [next-index (mod (inc current-index) (count carts))]
        (recur chart 
               (get-next-carts (assoc carts current-index {:pos next-pos, :value next-value, :turn next-turn}) next-index)
               next-index)))))
             
(defn print-chart [chart carts]
  (->> carts
       (reduce (fn [agg-chart cart] 
                 (assoc-in agg-chart (reverse (:pos cart)) (:value cart))) 
               chart)
       (map (partial apply str))
       (interpose "\n")
       (apply str)))


(defn adv25 []
  (let [{:keys [chart carts current-index]} input13]
    (find-collision chart carts current-index)))

(defn adv26 []
  (let [{:keys [chart carts current-index]} input13]
    (find-remaining-car chart carts current-index)))


; DAY 14

(defn get-initial-recipes [cnt]
  (let [v (vec (byte-array (+ cnt)))]
    {:cnt 2
     :data (assoc (assoc v 0 3) 1 7)
     :cur-a 0
     :cur-b 1
     :i 0}))
  
(defn next-recipes [{:keys [cnt data cur-a cur-b i]}] 
  (let [s (+ (data cur-a) (data cur-b))
        next-cnt (if (> s 9) (+ cnt 2) (+ cnt 1))
        r {:cnt next-cnt
           :cur-a (mod (+ cur-a (data cur-a) 1) next-cnt)
           :cur-b (mod (+ cur-b (data cur-b) 1) next-cnt)
           :i (inc i)}]
    (if (> s 9)
      (let [d (assoc (assoc data cnt (quot s 10))
                     (inc cnt)
                     (mod s 10))]
        (assoc r :data d))
      (assoc r          
             :data
             (assoc data cnt s)))))

(defn adv27 [cnt after]
  (->> (+ cnt after 1)
       (get-initial-recipes)
       (iterate next-recipes)
       (drop-while (fn [recipes] (< (:cnt recipes) (+ cnt after))))
       (first)
       (:data)
       (drop cnt)
       (take after)
       (apply str)))

(defn get-receipts-matcher [pattern]
  (let [cnt-p (count pattern)]
    (fn [{data :data, cnt :cnt}]
      (cond 
        (and (>= cnt cnt-p) (= (subvec data (- cnt cnt-p) cnt) pattern)) (- cnt cnt-p)
        (and (> cnt cnt-p) (= (subvec data (- cnt cnt-p 1) (dec cnt)) pattern)) (- cnt cnt-p 1)))))
  

(defn adv28 [pattern]
  (let [matcher (get-receipts-matcher pattern)]
    (->> 1000000
         (get-initial-recipes)
         (iterate next-recipes)
         (some matcher))))


