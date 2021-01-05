(ns adventofcode.day16
  (:require [clojure.string])
  (:require [adventofcode.commons :refer [label]]))

(def input16 (let [[ranges _ ticket _ tickets] (partition-by empty? (clojure.string/split-lines (slurp "input16")))]
               {:ranges ranges :ticket (second ticket) :tickets (rest tickets)}))

(:ticket input16)

(defn in-range? [v [lower1 upper1 lower2 upper2]]
  (or (<= lower1 v upper1)
      (<= lower2 v upper2)))

(defn parse-range [s]
  (let [[_ fname lower1 upper1 lower2 upper2] (first (re-seq #"(.*): ([0-9]*)-([0-9]*) or ([0-9]*)-([0-9]*)" s))]
    [fname (map read-string [lower1 upper1 lower2 upper2])]))

(defn parse-ticket [t]
  (map read-string (clojure.string/split t #",")))

(def data16 {:ranges (into {} (map parse-range (:ranges input16)))
             :ticket (parse-ticket (:ticket input16))
             :tickets (map parse-ticket (:tickets input16))})

(apply + (mapcat (fn [ticket] (remove (fn [v] (some (partial in-range? v) (vals (:ranges data16)))) ticket)) (:tickets data16)))

(def valid-tickets (remove (fn [ticket] (seq (remove (fn [v] (some (partial in-range? v) (vals (:ranges data16)))) ticket)))
                           (:tickets data16)))

(def keywords (range (count (:ticket data16))))
(def fields (into {} (map vector keywords (apply map vector valid-tickets))))

(:ranges data16)
(def cands (into {} (for [[namey rangey] (:ranges data16)] [namey (map first (filter (fn [field] (every? (fn [v] (in-range? v rangey)) (second field))) fields))])))

(apply * (map (fn [idx] (get (vec (:ticket data16)) idx)) (map second (filter (fn [[s _]] (clojure.string/starts-with? s "departure")) (label cands))) ))


