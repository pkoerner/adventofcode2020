(ns adventofcode.day5
  (:require [clojure.string]
            [clojure.set]))

(def boarding-passes (clojure.string/split-lines (slurp "input5"))) 


(defn binary-search
  ([cs upper lower-indicator upper-indicator] 
   (binary-search cs 0 upper lower-indicator upper-indicator))
  ([[c & r] lower upper lower-indicator upper-indicator]
   (cond
     (= lower upper) lower
     (= c lower-indicator) (recur r lower (quot (+ lower upper) 2) lower-indicator upper-indicator)
     (= c upper-indicator) (recur r (quot (+ lower upper 1) 2) upper lower-indicator upper-indicator) 
     :otherwise nil)))

(defn calculate-seat-id [bp]
  (let [[row-str column-str] (split-at 7 bp)]
    (+ (* (binary-search row-str 127 \F \B) 8)
       (binary-search column-str 7 \L \R))))

(def seats (map calculate-seat-id boarding-passes))
(reduce max seats)

(clojure.set/difference (set (range (reduce min seats) (reduce max seats))) seats)
