(ns sip.advent2019-day15)

(defn initialize-position [{:keys [terrain creatures] :as aa} [x y raw-value]]
  (condp = raw-value
    \# {:terrain (assoc-in terrain [y x] :wall), :creatures creatures}
    \. {:terrain (assoc-in terrain [y x] :space), :creatures creatures}
    \G (let [p {:id (count creatures), :hp 300, :type :goblin, :pos [x y]}]
         {:terrain (assoc-in terrain [y x] p), :creatures (conj creatures p)})
    \E  (let [p {:id (count creatures), :hp 300, :type :elf, :pos [x y]}]
         {:terrain (assoc-in terrain [y x] p), :creatures (conj creatures p)})
    (throw (Exception. "Unexpected object on the map"))))

(defn get-initial-state [raw-terrain]
  (let [maxy (count raw-terrain)
        maxx (count (first raw-terrain))
        empty-terrain (mapv (fn [row] (vec (repeat maxx :u)))
                            (range maxy))]
    (reduce initialize-position
            {:terrain empty-terrain, :creatures []}
            (mapcat (fn [y] (map (fn [x] [x y (get-in raw-terrain [y x])])
                                 (range maxx)))
                    (range maxy)))))

(def input
  (-> "src/sip/data/adv-input15-small.txt"
      (slurp)
      (clojure.string/split #"\n")
      (get-initial-state)))

(def directions [[0 -1] [-1 0] [1 0] [0 1]])

(defn find-next-creature-index [creatures current]
  (->> (range (inc current))
       (concat (range (inc current) (count creatures)))
       (filter (fn [i] (> (:hp (creatures i)) 0)))
       (first)))

(defn try-attack [{:keys [terrain creatures current], :as state}]
  (let [{ctype :type, [x y] :pos} (creatures current)
        target (->> directions
                    (map (fn [[dx dy]] (get-in terrain [(+ y dy) (+ x dx)])))
                    (filter :type)
                    (filter (partial not= ctype))
                    (sort-by :hp)
                    (first))]
     (when target
       (let [target-after (update-in target :hp #(- % 3))
             terrain-after (assoc-in terrain
                               (reverse (:pos target))
                               (if (> (:pos target-after) 0) target-after :space))
             creatures-after (assoc creatures (:id target) target-after)]
         {:terrain terrain-after
          :creatures creatures-after
          :current (find-next-creature-index creatures-after current)}))))

(defn find-start-move-to-target [moves]
  (->> moves 
      (filter (comp :dest :type))
      (sort (comp :start-pos reverse))
      (first)
      (:start-pos)))

(defn group-moves [] 
)      

; {[x y] #{[x y] [x' y]}
(defn get-possible-moves [terrain ctype visited current-positions]
  (let [get-neigbours (fn [x y start-pos]
    (->> directions 
         (map (fn [[dx dy]] 
            (let [dest-pos [( + x dx) (+ y dy)]]
              {:start-pos [x y], :dest-pos dest-pos, :dest (get-in terrain (reverse dest-pos))})))
         (filter (fn [[{dest :dest}]] (or (= dest :space) (= (:type dest) ctype))))))]
    (->> current-positions
         (keys)
         (mapcat (fn [[x y]] (get-neigbours x y (current-positions [x y])))))))


(defn move-creature [{:keys [terrain creatures current], :as state}]
)

(defn next-state [state]
  (let [new-state (try-attack state)]
    (if new-state
      new-state
      (move-creature state))))

(defn part1
  ([] part1 (assoc input :current 0))
  ([state]
    (->> state
         (iterate next-state))))

