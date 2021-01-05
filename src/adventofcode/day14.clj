(ns adventofcode.day14
  (:require [clojure.string]))

;; NOTE: I built my own representation for 36 bit integer values
;;       because I expected overflows to happen.
;;       They did not occur.

(def input14 (clojure.string/split-lines (slurp "input14")))

(defn to-36bit-bin
  ([n] (to-36bit-bin n 0 ()))
  ([n m acc] (if (= m 36) acc (recur (quot n 2) (inc m) (conj acc (mod n 2))))))

(count (to-36bit-bin 42))

(defn maskstr->list [s]
  {:instr :set-mask :data (map {\0 0 \1 1 \X nil} (drop 7 s))})

(defn assignment->data [s]
  {:instr :set-mem :data (map read-string (rest (first (re-seq #"mem\[([0-9]+)\] = ([0-9]+)" s))))})

(defn dataify [input]
  (for [line input]
    (if (clojure.string/starts-with? line "mask")
      (maskstr->list line)
      (assignment->data line))))


(def data14 (dataify input14))

(defn mask-data [mask data]
  (doall (map (fn [m d] (if m m d)) mask data)))

(defn expand-floating
  [c]
  (if (seq c)
    (if (nil? (first c))
      (concat (map (partial cons 0) (expand-floating (rest c)))
              (map (partial cons 1) (expand-floating (rest c))))
      (map (partial cons (first c)) (expand-floating (rest c)))) 
    [[]]))

(defn mask-addr [mask addr]
  (expand-floating (map (fn [m d] (if (= 0 m) d m)) mask (to-36bit-bin addr))))

(defn execute [data]
  (loop [mem {}
         mask nil
         [{:keys [instr data]} :as instrs] data]
    (if (seq instrs)
      (case instr
        :set-mask (recur mem data (rest instrs))
        :set-mem (recur (assoc mem (first data) (mask-data mask (to-36bit-bin (second data))))
                        mask (rest instrs)))
        mem)))

(defn execute2 [data]
  (loop [mem {}
         mask nil
         [{:keys [instr data]} :as instrs] data] (if (seq instrs)
      (case instr
        :set-mask (recur mem data (rest instrs))
        :set-mem (let [addrs (mask-addr mask (first data))]
                   (recur (reduce (fn [a addr] (assoc a addr (second data))) mem addrs)
                          mask
                          (rest instrs))))
        mem)))

(def resulting-mem (execute2 data14))
(defn the-36bit-to-int
  ([l] (the-36bit-to-int (reverse l) 1 0))
  ([l p acc] (if (seq l)
               (recur (rest l) (* 2 p) (+ acc (* p (first l))))
               acc)))

(reduce + (vals resulting-mem))
