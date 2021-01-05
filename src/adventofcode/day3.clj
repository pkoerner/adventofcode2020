(ns adventofcode.day3
  (:require [adventofcode.commons :refer [read-lines]]) )

(defn toboggan
  ([grid right down] (toboggan grid 0 0 right down))
  ([grid xpos n right down]
   (if (seq grid)
     (recur (drop down grid)
            (mod (+ right xpos) (count (first grid)))
            (if (= (get (first grid) xpos) \#)
              (inc n)
              n)
            right down)
     n)))

(* (toboggan (read-lines "input3") 1 1)
   (toboggan (read-lines "input3") 3 1)
   (toboggan (read-lines "input3") 5 1)
   (toboggan (read-lines "input3") 7 1)
   (toboggan (read-lines "input3") 1 2))

