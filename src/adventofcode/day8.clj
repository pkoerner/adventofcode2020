(ns adventofcode.day8
  (:require [clojure.string]))

(def instructions (vec (map (fn [x] (read-string (str \[ x \]))) (clojure.string/split-lines (slurp "input8")))))

(defn interpret-once
  ([instructions] (interpret-once instructions 0 0 #{}))
  ([instructions pc acc seen]
   (if (contains? seen pc)
     nil
     (if (= pc (count instructions))
       acc
       (let [[cmd arg] (get instructions pc)]
         (cond
           (= cmd 'acc) (recur instructions (inc pc) (+ acc arg) (conj seen pc))
           (= cmd 'jmp) (recur instructions (+ pc arg) acc (conj seen pc))
           (= cmd 'nop) (recur instructions (inc pc) acc (conj seen pc))
           :otherwise :oof))))))

(defn calc-patches [instructions]
  (remove #{instructions}
          (loop [i 0
                 seen #{}]
            (if (= (count instructions) i)
              seen
              (recur (inc i) (conj seen (update-in instructions [i 0] {'nop 'jmp, 'acc 'acc, 'jmp 'nop})))))))

(keep interpret-once (calc-patches instructions))
