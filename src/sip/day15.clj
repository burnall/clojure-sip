(ns sip.day15)
;(require 'sip.day15 :reload)

; Put a wall, a space, a creature into terrain base on raw value
(defn initialize-terrain-point [{:keys [terrain creatures]} [x y raw-value]]
  (condp = raw-value
    \# {:terrain (assoc-in terrain [y x] :wall), :creatures creatures}
    \. {:terrain (assoc-in terrain [y x] :space), :creatures creatures}
    \G (let [cid (count creatures) 
             creature {:hp 300, :type :goblin, :pos [x y]}]
         {:terrain (assoc-in terrain [y x] {:cid cid}), :creatures (conj creatures creature)})
    \E (let [cid (count creatures) 
             creature {:hp 300, :type :goblin, :pos [x y]}]
         {:terrain (assoc-in terrain [y x] {:cid cid}), :creatures (conj creatures creature)})
    (throw (Exception. "Unexpected object on the map"))))

; Convert two-dimensional raw terrain into 
; State {:terrain :creatures :done :current-creater-index :tick :round}
; tick - move number (creature change - position or hit points)
; round - increases as all alive creatures moved once (or skip if blocked). 
(defn get-initial-state [raw-terrain]
  (let [maxy (count raw-terrain)
        maxx (count (first raw-terrain))
        empty-terrain (mapv (fn [row] (vec (repeat maxx :u)))
                            (range maxy))]
    (reduce initialize-terrain-point
            {:terrain empty-terrain, :creatures [], :done false, :current-index 0, :tick 0, :round 0}
            (mapcat (fn [y] (map (fn [x] [x y (get-in raw-terrain [y x])])
                                 (range maxx)))
                    (range maxy)))))

(defn terrain-sketch [{:keys [terrain creatures]}]
  (let [get-ch]
  (->> terrain
       (map (fn [row] (reduce  



; Get state as above
; {:terrain [[] []], :creatures [{:id :hp :type :pos}] }
(def input
  (-> "src/sip/data/adv-input15-small.txt"
      (slurp)
      (clojure.string/split #"\n")
      (get-initial-state)))

(defn next-state [state])


(defn roll [initial-state])

