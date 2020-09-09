(ns sip.advent2019-day15)

; Put a wall, a space, a creature into terrain base on raw value
(defn initialize-terrain-point [{:keys [terrain creatures]} [x y raw-value]]
  (condp = raw-value
    \# {:terrain (assoc-in terrain [y x] :wall), :creatures creatures}
    \. {:terrain (assoc-in terrain [y x] :space), :creatures creatures}
    \G (let [creature {:id (count creatures), :hp 300, :type :goblin, :pos [x y]}]
         {:terrain (assoc-in terrain [y x] creature), :creatures (conj creatures creature)})
    \E  (let [creature {:id (count creatures), :hp 300, :type :elf, :pos [x y]}]
         {:terrain (assoc-in terrain [y x] creature), :creatures (conj creatures creature)})
    (throw (Exception. "Unexpected object on the map"))))

; Convert two-dimensional raw terrain into State {:terrain :creatures}
(defn get-initial-state [raw-terrain]
  (let [maxy (count raw-terrain)
        maxx (count (first raw-terrain))
        empty-terrain (mapv (fn [row] (vec (repeat maxx :u)))
                            (range maxy))]
    (reduce initialize-terrain-point
            {:terrain empty-terrain, :creatures []}
            (mapcat (fn [y] (map (fn [x] [x y (get-in raw-terrain [y x])])
                                 (range maxx)))
                    (range maxy)))))

; Get state
; {:terrain [[] []], :creatures [{:id :hp :type :pos}] }
(def input
  (-> "src/sip/data/adv-input15-small.txt"
      (slurp)
      (clojure.string/split #"\n")
      (get-initial-state)))

(def directions [[0 -1] [-1 0] [1 0] [0 1]])

; Find next alive creature starting from (current + 1) position and then circularly back to zero position  
(defn find-next-creature-index [creatures current]
  (->> (range (inc current))
       (concat (range (inc current) (count creatures)))
       (filter (fn [i] (> (:hp (creatures i)) 0)))
       (first)))

; Return new state after attack if it possible (a target in adjacent cell)
; Otherwise return nil
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
          :current (find-next-creature-index creatures-after current)
          :last-moved current}))))

; Find  [{:start-positions, :dest-pos, :dest}]
(defn find-attacking-move [possible-moves]
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

; Input - {[x y] #{[x0 y0] [x0' y0']} - map key - a destination point, value - vector of starting positions
; [{:start-positions, :dest-pos, :dest}] 
; TODO: fix keys
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

; Returning a starting point if enemy is reachable or nil otherwise
(defn search-for-target [terrain enemy-type visited current-positions]
  (let [possible-moves (get-possible-moves terrain enemy-type visited current-positions)
        start-move (find-attacking-move possible-moves)]
    (if start-move 
      start-move
      (let [moves (group-moves possible-moves)
            visited-next (apply conj visited (keys moves))]
          (recur terrain enemy-type visited-next moves)))))

; Return possible moves from the point as {move #{move}}
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
    (search-for-target terrain enemy-type initial-visited initial-positions)))

;
; state keys - :terrain, :creatures, :current, :last-moved, :done
(defn next-state [{:keys [last-moved current terrain creatures] :as state}]
  (if (= last-moved current) 
    (assoc state :done true)
    (if-let [state-after (try-attack state)]
      state-after
      (let [move (move-creature state)
        {pos-b :pos, :as creature} (creatures current)
        creature-after (assoc creature :pos move)
        creatures-after (assoc creatures current creature-after)
        terrain-after (assoc-in terrain (reverse pos-b) :space)
        terrain-after (assoc-in terrain-after (reverse move) creature-after)]
        {:terrain terrain-after 
         :creatures creatures-after
         }
        move))))
; change pos in creature
; terrain: set :space to old pos, put creature into new pos
; change creature
; change last-moved
; if no move set done


(defn part1
  ([] (part1 (assoc input :current 0)))
  ([state]
    (->> state
         (iterate next-state)
         (second))))

