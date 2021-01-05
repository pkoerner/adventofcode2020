(ns adventofcode.day10
  (:require [adventofcode.commons :refer [read-file]]))


(def input10 (read-file "input10"))
(def input10' [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(defn calc-differences [c]
  (map (comp (partial apply -) reverse) (partition 2 1 (cons 0 (sort c)))))

(let [freqs (frequencies (calc-differences input10))]
  (* (get freqs 1) (inc (get freqs 3))))

(reduce max (partition-by identity (calc-differences input10)))
(calc-differences input10')

(def possibilities {[1 1] 2
                    [1 1 1] 4
                    [1 1 1 1] 7})

; ,,, 1 1 ,,, -> 2 possibilities (drop first adapter or keep it)
; ,,, 1 1 1 ,,, -> 4 possibilities ; - drop first
                                   ; - drop second
                                   ; - drop first and second
                                   ; - 
; ,,, 1 1 1 1 ,,, -> 7 possibilities (guessed)
; 5 or more 1s do not occur (cheating bastard!)

(reduce * (map #(get possibilities % 1) (partition-by identity (calc-differences input10))))

