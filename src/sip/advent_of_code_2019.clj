(ns sip.advent-of-code-2019)

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
  
