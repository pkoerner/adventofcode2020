(ns adventofcode.day12
  (:require [clojure.string]))

(def instr (map (fn [[c & n-str]] [c (read-string (apply str n-str))]) (clojure.string/split-lines (slurp "input12"))))

(def left-turn {\N \W, \W \S, \S \E, \E \N})
(def right-turn {\N \E, \E \S, \S \W, \W \N})

(defmulti move (fn [_ [instr _]] instr))
(defmethod move \N [[x y dir] [_ n]] [x (+ y n) dir])
(defmethod move \S [[x y dir] [_ n]] [x (- y n) dir])
(defmethod move \E [[x y dir] [_ n]] [(+ x n) y dir])
(defmethod move \W [[x y dir] [_ n]] [(- x n) y dir])
(defmethod move \L [[x y dir] [_ n]] 
  (if (= n 90)
    [x y (left-turn dir)]
    (move [x y (left-turn dir)] [\L (- n 90)])))
(defmethod move \R [[x y dir] [_ n]] [x y dir]
  (if (= n 90)
    [x y (right-turn dir)]
    (move [x y (right-turn dir)] [\R (- n 90)])))
(defmethod move \F [[x y dir] [_ n]]
  (move [x y dir] [dir n]))

(def coords (reduce move [0 0 \E] instr))

(defn manhattan-distance [[x y _]]
  (+ (Math/abs x) (Math/abs y)))

(manhattan-distance coords)

(defmulti move2 (fn [_ [instr _]] instr))
(defmethod move2 \N [[x y wx wy] [_ n]] [x y wx (+ wy n)])
(defmethod move2 \S [[x y wx wy] [_ n]] [x y wx (- wy n)])
(defmethod move2 \E [[x y wx wy] [_ n]] [x y (+ wx n) wy])
(defmethod move2 \W [[x y wx wy] [_ n]] [x y (- wx n) wy])
(defmethod move2 \L [[x y wx wy] [_ n]] 
  (if (= n 0)
    [x y wx wy]
    (move2 [x y (- wy) wx] [\L (- n 90)])))
(defmethod move2 \R [[x y wx wy] [_ n]]
  (if (= n 0)
    [x y wx wy]
    (move2 [x y wy (- wx)] [\R (- n 90)])))
(defmethod move2 \F [[x y wx wy] [_ n]]
  [(+ x (* n wx)) (+ y (* n wy)) wx wy])

(def coords2 (reduce move2 [0 0 10 1 \E] instr))
(manhattan-distance coords2)
