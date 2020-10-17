(ns sip.digit-search)

(defn diff-digits [x]
  (->> [x #{}]
       (iterate (fn [[x agg]] 
                  [(quot x 10) (conj agg (mod x 10))]))
       (some (fn [[x agg]] (when (zero? x) agg)))))

(def diff-digits2 (comp set str))
 

(defn digit-search [xs]
  (->> xs 
       (reductions (fn [agg x] 
                     (clojure.set/union agg (diff-digits x)))   
                   #{})
       (map (fn [x agg] [x agg]) xs)
       (some (fn [[x agg]] (when (= 10 (count agg)) x)))))

(defn digit-search2 [xs] 
  (loop [[x & others] xs  
          all (diff-digits2 1234567890)]
    (when x
      (let [more-digits (clojure.set/difference all (diff-digits2 x))]
        (if (seq more-digits)
          (recur others more-digits)
          x)))))
