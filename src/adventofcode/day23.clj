(ns adventofcode.day23)

(def crab-cups [1 3 7 8 2 6 4 9 5])
;(def crab-cups [3 8 9 1 2 5 4 6 7])
(def crab-cups2 (concat crab-cups (remove (set crab-cups) (range 1 (inc 1000000)))))

(defn calc-dest [current not-these n]
  (first (remove (set not-these) 
                 (map (fn [x] (if (< x 1) (+ n x) x))
                      (range (dec current) (- current 5) -1)))))

(def a (atom 0))
(defn crab-game [cups current-cup]
  (when (= 0 (mod @a 100000)) (prn @a))
  (swap! a inc)
  (let [[left-side right-side] (split-with (complement #{current-cup}) (take (+ 3 (count cups)) (cycle cups)))
        three-cups (take 3 (drop 1 right-side))
        six-cups (remove (set three-cups) cups)
        dest (calc-dest current-cup three-cups (count cups))
        dest-pos (inc (.indexOf six-cups dest))
        newcups (concat (take dest-pos six-cups) three-cups (drop dest-pos six-cups))]
    ;(prn :cups cups :current current-cup :tree three-cups :dest dest)
    [newcups (nth newcups (mod (inc (.indexOf newcups current-cup)) (count cups)))]))

(defn crabbo-game [m current-cup]
  (when (= 0 (mod @a 100000)) (prn @a))
  (swap! a inc)
  (let [cup1 (get m current-cup)
        cup2 (get m cup1)
        cup3 (get m cup2)
        next-cup (get m cup3)
        m (assoc m current-cup next-cup)
        dest (calc-dest current-cup [cup1 cup2 cup3] (count m))
        tmp (get m dest)
        m (assoc m dest cup1)
        m (assoc m cup3 tmp)
        next-cup (get m current-cup)]
    [m next-cup]))

(def crab-cups-m (into {} (map vec (partition 2 1 (concat crab-cups [(first crab-cups)])))))
(def res (last (take (inc 100) (iterate (fn [x] (apply crabbo-game x)) [crab-cups-m 1]))))

(def crab-cups-m (into {} (map vec (partition 2 1 (concat crab-cups2 [(first crab-cups2)])))))
(def res (last (take (inc 10000000) (iterate (fn [x] (apply crabbo-game x)) [crab-cups-m 1]))))

;(def res (last (take (inc 10000000) (iterate (fn [x] (apply crab-game x)) [crab-cups2 1]))))
;(take 3 (apply concat (reverse (split-with (complement #{1}) (first res)))))

(* (->> 1 (get (first res)) (get (first res))) (get (first res) 1))
;(get res 1)
;(get res 300)
;(count res)
