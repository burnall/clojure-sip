(ns sip.advent2019-day15)

(defn initialize-position [{:keys [terrain persons] :as aa} [x y raw-value]]
  (condp = raw-value
    \# {:terrain (assoc-in terrain [y x] :wall), :persons persons}
    \. {:terrain (assoc-in terrain [y x] :space), :persons persons}
    \G (let [p {:id (count persons), :hp 300, :type :goblin, :pos [x y]}]
         {:terrain (assoc-in terrain [y x] p), :persons (conj persons p)})
    \E  (let [p {:id (count persons), :hp 300, :type :elf, :pos [x y]}]
         {:terrain (assoc-in terrain [y x] p), :persons (conj persons p)})
    (throw (Exception. "Unexpected object on the map"))))

(defn get-initial-state [raw-terrain]
  (let [maxy (count raw-terrain)
        maxx (count (first raw-terrain))
        empty-terrain (mapv (fn [row] (vec (repeat maxx :u)))
                            (range maxy))]
    (reduce initialize-position
            {:terrain empty-terrain, :persons []}
            (mapcat (fn [y] (map (fn [x] [x y (get-in raw-terrain [y x])])
                                 (range maxx)))
                    (range maxy)))))

(def input
  (-> "src/sip/data/adv-input15-small.txt"
      (slurp)
      (clojure.string/split #"\n")
      (get-initial-state)))

(defn part1 [] )

