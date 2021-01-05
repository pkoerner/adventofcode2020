(ns adventofcode.day24
  (:require [clojure.string]))

(def input24 (clojure.string/split-lines (slurp "input24")))

(defn calc-pos [[dir1 dir2 & dirs :as s] x y]
  (if (seq s)
    (case dir1
      \e (recur (when dir2 (cons dir2 dirs)) (+ x 2) y)
      \w (recur (when dir2 (cons dir2 dirs)) (- x 2) y)
      \s (recur dirs ((if (= \e dir2) inc dec) x) (dec y))
      \n (recur dirs ((if (= \e dir2) inc dec) x) (inc y)))
    [x y]))


(calc-pos "nwwswee" 0 0)

(count (filter (fn [[_ v]] (odd? v)) (frequencies (map #(calc-pos % 0 0) input24))))
(defn touching-poss [x y]
  [[(+ x 2) y]
   [(- x 2) y]
   [(inc x) (dec y)]
   [(dec x) (dec y)]
   [(inc x) (inc y)]
   [(dec x) (inc y)]])

(def starting-black-tiles (set (map first (filter (fn [[_ v]] (odd? v)) (frequencies (map #(calc-pos % 0 0) input24))))))

(defn step [blacks]
  (let [full-grid (into (set (mapcat (partial apply touching-poss) blacks)) blacks)]
    (set (for [pos full-grid
               :let [neighbours (apply touching-poss pos)]
               :when (or (and (contains? blacks pos)
                              (#{1 2} (count (filter blacks neighbours))))
                         (and (not (contains? blacks pos))
                              (= 2 (count (filter blacks neighbours)))))]
           pos))))

(count (last (take 101 (iterate step starting-black-tiles))))

