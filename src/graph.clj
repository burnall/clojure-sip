 (ns graph)

; graph 1

(let [read-pair (fn[] (map read-string (clojure.string/split (read-line) #"\s")))
      [v e] (read-pair)
      edges (repeatedly e read-pair)
      upd (fn[m k f] (assoc m k (f (get m k))))
      updateMap (fn [m [a b]] (upd m a #(if (nil? %) [b] (conj % b))))
      graph (reduce (fn[g [a b]] (updateMap (updateMap g [a b]) [b a])) {} edges)
      ;graph {1 [3 2], 3 [1 4], 2 [1], 4 [3]}

      trip (fn[v visited]
              (reduce #(if (% %2) % (recur %2 (conj % %2)))
                      visited
                      (graph v)))
      visit (fn[[visited count] v] (if (visited v) [visited count] [(trip v (conj visited v)) (inc count)]))]

  (println (second (reduce visit [#{} 0] (range 1 (inc v))))))
