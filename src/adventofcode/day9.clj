(ns adventofcode.day9
  (:require [adventofcode.commons :refer [read-file]]) )

(def input9 (read-file "input9"))
(def partitions (partition 26 1 input9))

(defn valid-partition [c]
  (let [n (last c)
        cands (butlast c)]
    (seq (for [a cands b cands :when (= n (+ a b))] (+ a b)))))

(def target (last (first (remove valid-partition partitions))))

(defn find-contiguous-seq [c target]
  (loop [c c
         d (rest c)
         acc (first c)
         s [(first c)]]
    (let [nacc (+ acc (first d))]
      (cond (= nacc target) (conj s (first d))
            (> nacc target) (recur (rest c) (drop 2 c) (first (rest c)) [(first (rest c))])
            :otherwise      (recur c (rest d) nacc (conj s (first d)))))))

(let [subset (find-contiguous-seq input9 target)]
  (+ (reduce max subset) (reduce min subset)))
