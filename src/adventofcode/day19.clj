(ns adventofcode.day19
  (:require [clojure.string]))

(def input19 (clojure.string/split-lines (slurp "input19")))
(let [[rules [_ & inputs]]  (split-with seq input19)]
  (def rules19 rules)
  (def input19 inputs))

(first rules19)
(defn process-rule-str [s]
  (let [[[_ rulenum r]] (re-seq #"([0-9]*): (.*)" s)]
    [(read-string rulenum)
     (map (fn [s] (read-string (str \[ s \]) )) (clojure.string/split r #"\|"))]))


(def rules19' (into {} (map process-rule-str rules19)))
(def rules19' (assoc (into {} (map process-rule-str rules19))
                     8 [[42] [42 8]]
                     11 [[42 31] [42 11 31]]))

(defn interp [x w]
  (if (seq x)
    (if (number? (first x))
      (let [alts (get rules19' (first x))]
        (some (fn [xx] (interp xx w)) (map (fn [a] (concat a (rest x))) alts)))
      (if (= (ffirst x) (first w))
        (recur (rest x) (rest w))
        false))
    (empty? w)))

(count (keep  (partial interp [0]) input19))

