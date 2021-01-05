(ns adventofcode.day17
  (:require [clojure.string]))

(def input17 (clojure.string/split-lines (slurp "input17")))
(def data17 {:x-lo 0
             :x-up (count (first input17)) ; exclusive
             :y-lo 0
             :y-up (count input17) ; exclusive
             :z-lo 0
             :z-up 1 ; exclusive
             :w-lo 0
             :w-up 1 ; exclusive
             :field (into #{} (for [y (range (count input17))
                                    x (range (count (first input17)))
                                    :when (= \# (get-in input17 [y x]))]
                                [x y 0 0]))})

(defn neighbours [[x y z]]
  (for [x' [(dec x) x (inc x)]
        y' [(dec y) y (inc y)]
        z' [(dec z) z (inc z)]
        :when (not= [x y z] [x' y' z'])]
    [x' y' z']))

(defn neighbours2 [[x y z w]]
  (for [x' [(dec x) x (inc x)]
        y' [(dec y) y (inc y)]
        z' [(dec z) z (inc z)]
        w' [(dec w) w (inc w)]
        :when (not= [x y z w] [x' y' z' w'])]
    [x' y' z' w']))

(defn step [{:keys [x-lo x-up y-lo y-up z-lo z-up field]}]
  {:x-lo (dec x-lo)
   :x-up (inc x-up)
   :y-lo (dec y-lo)
   :y-up (inc y-up)
   :z-lo (dec z-lo)
   :z-up (inc z-up)
   :field (into #{} (for [x (range (dec x-lo) (inc x-up))
                          y (range (dec y-lo) (inc y-up))
                          z (range (dec z-lo) (inc z-up))
                          :let [n-neighbours (count (filter (comp field #(conj % 0)) (neighbours [x y z])))]
                          :when (or (and (get field [x y z]) (<= 2 n-neighbours 3))
                                    (= 3 n-neighbours))]
                      [x y z 0]))})

(defn step2 [{:keys [x-lo x-up y-lo y-up z-lo z-up w-lo w-up field]}]
  (println :step)
  {:x-lo (dec x-lo)
   :x-up (inc x-up)
   :y-lo (dec y-lo)
   :y-up (inc y-up)
   :z-lo (dec z-lo)
   :z-up (inc z-up)
   :w-lo (dec w-lo)
   :w-up (inc w-up)
   :field (into #{} (for [x (range (dec x-lo) (inc x-up))
                          y (range (dec y-lo) (inc y-up))
                          z (range (dec z-lo) (inc z-up))
                          w (range (dec z-lo) (inc z-up))
                          :let [n-neighbours (count (filter field (neighbours2 [x y z w])))]
                          :when (or (and (get field [x y z w]) (<= 2 n-neighbours 3))
                                    (= 3 n-neighbours))]
                      [x y z w]))})

(def boot-state (first (drop 6 (iterate step2 data17))))
(count (:field boot-state))
