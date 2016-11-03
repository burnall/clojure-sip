 (ns graph2)

(let [
      read-pair (fn[] (map read-string (clojure.string/split (read-line) #"\s")))
      [v e] (read-pair)
      edges (repeatedly e read-pair)
      upd (fn[m k f] (assoc m k (f (get m k))))
      updateMap (fn [m [a b]] (upd m a #(if (nil? %) [b] (conj % b))))
      graph (reduce (fn[g [a b]] (updateMap (updateMap g [a b]) [b a])) {} edges)
      ;graph {0 [1 2], 1 [0 3], 2 [0], 3 [1]}

      neighbours (fn[v visited vis2]
                   (mapcat #(when-not (or (visited %) (vis2 %)) [%]) (graph v)))
      invade (fn[all sub i]
             (let [
                   newSub (reduce (fn[s v] (into s (neighbours v all s))) #{} sub)
                   ]
               (if (empty? newSub)
                 all
                 (recur (into all (apply assoc {} (interleave newSub (repeat i)))) newSub (inc i)))))
      ]

  (do (graph 0) (print 0 1 2 2 3)))
  ;(apply print (map second (invade (sorted-map 0 0) #{0} 1))))
