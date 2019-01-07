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

(def directions [[0 -1] [-1 0] [1 0] [0 1]])

(defn find-next-person-index [persons current]
  (->> (range (inc current))
       (concat (range (inc current) (count persons)))
       (filter (fn [i] (> (:hp (persons i)) 0)))
       (first)))

(defn try-attack [{:keys [terrain persons current], :as state}]
  (let [{ptype :type, [x y] :pos} (persons current)
        target (->> directions
                    (map (fn [[dx dy]] (get-in terrain [(+ y dy) (+ x dx)])))
                    (filter :type)
                    (filter (partial not= ptype))
                    (sort-by :hp)
                    (first))]
     (when target
       (let [target-after (update-in target :hp #(- % 3))
             terrain-after (assoc-in terrain
                               (reverse (:pos target))
                               (if (> (:pos target-after-attack) 0) target-after :space))
             persons-after (assoc persons (:id target) target-after)]
         {:terrain terrain-after
          :persons persons-after
          :current (find-next-person-index persons-after current)}))))

(defn move-person [{:keys [terrain persons current]}]
  )

(defn next-state [state]
  (let [new-state (try-attack state)]
    (if new-state
      new-state
      (move-person state))))

(defn part1
  ([] part1 (assoc input :current 0))
  ([state]
    (->> state
         (iterate next-state))))

