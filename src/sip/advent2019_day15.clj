(ns sip.advent2019-day15)

(defn initialize-position [{:keys [terrain creatures] :as aa} [x y raw-value]]
  (condp = raw-value
    \# {:terrain (assoc-in terrain [y x] :wall), :creatures creatures}
    \. {:terrain (assoc-in terrain [y x] :space), :creatures creatures}
    \G (let [creature {:id (count creatures), :hp 300, :type :goblin, :pos [x y]}]
         {:terrain (assoc-in terrain [y x] creature), :creatures (conj creatures creature)})
    \E  (let [creature {:id (count creatures), :hp 300, :type :elf, :pos [x y]}]
         {:terrain (assoc-in terrain [y x] creature), :creatures (conj creatures creature)})
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

(defn find-start-move-to-target [possible-moves]
  (->> possible-moves 
      (filter (comp :type :dest))
      (mapcat :start-positions)
      (sort-by (comp vec reverse))
      (first)))

(defn group-moves [possible-moves]
  (->> possible-moves
       (group-by :dest-pos)
       (map (fn [[dest-pos grouped]] 
         [dest-pos 
          (apply clojure.set/union (map (comp set :start-positions) grouped))]))
       (into {})))

; {[x y] #{[x0 y0] [x0' y0']}
(defn get-possible-moves [terrain enemy-type visited current-positions]
  (let [get-neigbour-moves (fn [x y start-positions]
    (->> directions 
         (map (fn [[dx dy]] 
           (let [dest-pos [( + x dx) (+ y dy)]]
             {:start-positions start-positions, :dest-pos dest-pos, :dest (get-in terrain (reverse dest-pos))})))
         (filter (fn [{dest :dest}] (or (= dest :space) (= (:type dest) enemy-type))))
         (filter (comp not visited :dest-pos))))]
    (->> current-positions
         (keys)
         (mapcat (fn [[x y]] (get-neigbour-moves x y (current-positions [x y])))))))


(defn search-for-target [terrain enemy-type visited current-positions cnt]
  (let [possible-moves (get-possible-moves terrain enemy-type visited current-positions)
        start-move (find-start-move-to-target possible-moves)]
    (if start-move 
      start-move
      (let [moves (group-moves possible-moves)
            visited-next (apply conj visited (keys moves))]
        (if (zero? cnt) 
          33
          (recur terrain enemy-type visited-next moves (dec cnt)))))))

(defn get-initial-current-positions [terrain [x y]] 
  (->> directions
       (map (fn [[dx dy]]
         (let [p [(+ x dx)  (+ y dy)]]
           {:pos p, :value (get-in terrain (reverse p))})))
       (filter (fn [{v :value}] (= v :space)))
       (map (fn [{p :pos}] [p #{p}]))
       (into {})))

(defn move-creature [{:keys [terrain creatures current], :as state}]
  (let [{ctype :type, pos :pos} (creatures current)
        enemy-type ({:goblin :elf, :elf :goblin} ctype)
        initial-positions (get-initial-current-positions terrain pos)
        initial-visited (into #{pos} (map :pos initial-positions))]
    (search-for-target terrain enemy-type initial-visited initial-positions -1)))

(defn next-state [state]
  (let [state-after (try-attack state)]
    (if state-after
      state-after
      (move-creature state))))

(defn part1
  ([] (part1 (assoc input :current 0)))
  ([state]
    (->> state
         (iterate next-state)
         (second))))

