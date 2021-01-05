(ns adventofcode.day22
  (:require [clojure.string]))

(def input22 (clojure.string/split-lines (slurp "input22")))
(def data22 (let [[p1 b] (split-with seq (rest input22))
                  p2 (drop 2 b)]
              {:player1 (vec (map read-string p1))
               :player2 (vec (map read-string p2))}))

;; combat seems to be a less violent version of
;; https://github.com/gigasquid/wonderland-clojure-katas/tree/master/card-game-war
(defn combat [[c1 & d1 :as player1] [c2 & d2 :as player2]]
  (if (and (seq player1) (seq player2))
    (if (> c1 c2)
      (recur (conj (vec d1) c1 c2) d2)
      (recur d1 (conj (vec d2) c2 c1)))
    [player1 player2]))

(def res (combat (:player1 data22) (:player2 data22)))
(reduce + (map * (first res) (range 50 0 -1)))


(defn recursive-combat
  ([p1 p2] #_(prn :new-game (+ (count p1) (count p2))) (recursive-combat p1 p2 #{} #{}))
  ([[c1 & d1 :as player1] [c2 & d2 :as player2] seen1 seen2]
   (if (and (seq player1) (seq player2))
     (if (or (seen1 player1) (seen2 player2))
       [player1 player2 :p1-win]
       (if (and (<= c1 (count d1))
                (<= c2 (count d2)))
         ;; recurse
         (let [[_ _ res] (recursive-combat (take c1 d1) (take c2 d2))]
           (if (= :p1-win res)
             (recur (conj (vec d1) c1 c2) d2 (conj seen1 player1) (conj seen2 player2))
             (recur d1 (conj (vec d2) c2 c1) (conj seen1 player1) (conj seen2 player2))))
         (if (> c1 c2)
           (recur (conj (vec d1) c1 c2) d2 (conj seen1 player1) (conj seen2 player2))
           (recur d1 (conj (vec d2) c2 c1) (conj seen1 player2) (conj seen2 player2)))))
     [player1 player2 (if (seq player1) :p1-win :p2-win)])))

(def res (recursive-combat (:player1 data22) (:player2 data22)))
(def res (recursive-combat [9 2 6 3 1] [5 8 4 7 10]))
(reduce + (map * (first res) (range 50 0 -1)))
