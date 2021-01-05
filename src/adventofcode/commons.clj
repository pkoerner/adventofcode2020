(ns adventofcode.commons
  (:require [clojure.string]))

(defn read-file [x]
  (read-string (str \[ (slurp x) \])))

(defn read-lines [x]
  (clojure.string/split (slurp x) #"\n"))

(defn read-groups [filename]
  (remove (fn [x] (= x [""])) (partition-by empty? (clojure.string/split-lines (slurp filename)))))

;; this is a variation of a topological sorting algorithm
;; that I use as an exercise in my course
(defn label [m]
  (if (seq m)
    (let [[fieldname [kw]] (first (filter (fn [[k v]] (= 1 (count v))) m))
          next-cands (into {} (map (fn [[k v]] [k (remove #{kw} v)]) (dissoc m fieldname)))]
      (assert (< (count next-cands) (count m)))
      (cons [fieldname kw]
            (label next-cands)))
    ()))
