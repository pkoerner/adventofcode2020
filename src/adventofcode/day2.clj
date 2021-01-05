(ns adventofcode.day2
  (:require [clojure.string]))


(def input2 (slurp "input2"))
(def input2-lines (clojure.string/split input2 #"\n"))

(defn password-check [line]
  (let [[mins maxs [c] _ pw] (clojure.string/split line #"-| |:")
        mini (Integer/parseInt mins)
        maxi (Integer/parseInt maxs)
        freqs (frequencies pw)]
    (<= mini (get freqs c 0) maxi)))

(defn xor [a b]
  (and (or a b)
       (not (and a b))))

(defn password-check2 [line]
  (let [[mins maxs [c] _ pw] (clojure.string/split line #"-| |:")
        mini (Integer/parseInt mins)
        maxi (Integer/parseInt maxs)]
    (xor (= (get pw (dec mini)) c)
         (= (get pw (dec maxi)) c))))

(count (filter password-check2 input2-lines))
