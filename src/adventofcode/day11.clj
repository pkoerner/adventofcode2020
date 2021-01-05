(ns adventofcode.day11
  (:require [clojure.string]))

(def start-seats (clojure.string/split-lines (slurp "input11")))

(defn get-neighbours [seats y x]
  (remove nil? [(get-in seats [(inc y) x])
                (get-in seats [(inc y) (inc x)])
                (get-in seats [y       (inc x)])
                (get-in seats [(dec y) (inc x)])
                (get-in seats [(dec y) x])
                (get-in seats [(dec y) (dec x)])
                (get-in seats [y       (dec x)])
                (get-in seats [(inc y) (dec x)])]))

(get-neighbours start-seats 5 0)

(defn get-visible-neighbours [seats y x]
  (remove nil? (map 
                 (fn [f] (first (drop-while (fn [seat] (#{\.} seat))
                                            (map (fn [[y x]] (get-in seats [y x]))
                                                 (drop 1 (iterate f [y x]))))))
                 [(fn [[y x]] [(inc y) x])
                  (fn [[y x]] [(inc y) (inc x)])
                  (fn [[y x]] [y       (inc x)])
                  (fn [[y x]] [(dec y) (inc x)])
                  (fn [[y x]] [(dec y) x])
                  (fn [[y x]] [(dec y) (dec x)])
                  (fn [[y x]] [y       (dec x)])
                  (fn [[y x]] [(inc y) (dec x)])])))

(defn step [seats thresh neighbour-fn]
  (let [width (count (first seats))
        height (count seats)]
    (vec (for [h (range height)]
           (vec (for [w (range width)]
                  (let [state (get-in seats [h w])
                        neighbours (neighbour-fn seats h w)]
                    (cond (and (= state \L) (every? (complement #{\#}) neighbours)) \#
                          (and (= state \#) (>= (count (filter #{\#} neighbours)) thresh)) \L
                          :else state))))))))


;; as taught in my course :-)
(defn fixedpoint [f start eps?]
  (loop [x start]
    (let [next-x (f x)]
      (if (eps? x next-x)
        next-x
        (recur next-x)))))


(def fix  (fixedpoint (fn [x] (step x 4 get-neighbours)) start-seats =))
(def fix2 (fixedpoint (fn [x] (step x 5 get-visible-neighbours)) start-seats =))
(count (filter #{\#} (apply concat fix2)))
