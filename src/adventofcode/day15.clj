(ns adventofcode.day15)

(defn memory-start [x y z]
  [z 3 {x 1, y 2}]) 

(defn memory-update [[prev-turn prev-turn-num mem]]
  (let [turn-num (get mem prev-turn 0)]
    [(if (zero? turn-num) 0 (- prev-turn-num turn-num)) (inc prev-turn-num) (assoc mem prev-turn prev-turn-num)]))

(ffirst (drop-while #(not= 30000000 (second %)) (iterate memory-update [2 6 {8 1, 11 2, 0 3, 19 4, 1 5}])))
