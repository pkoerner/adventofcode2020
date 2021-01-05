(ns adventofcode.day21
  (:require [clojure.string])
  (:require [adventofcode.commons :refer [label]]) )

(def input21 (clojure.string/split-lines (slurp "input21")))

(defn allergens-ingredients [s]
  (let [[ingr aller'] (split-with (complement #{"(contains"}) (clojure.string/split s #" "))]
    [(set (map (comp (partial apply str) butlast) (rest aller'))) (set ingr)]))

(def allergens-cands (map allergens-ingredients input21))
(def allergens (apply clojure.set/union (map first allergens-cands)))

(def new-cands (for [a allergens]
                 [a (apply clojure.set/intersection (map second (filter (fn [[al in]] (al a)) allergens-cands)))]))

(count (remove (apply clojure.set/union (map second new-cands))
        (apply concat (map second allergens-cands))))

(clojure.string/join "," (map second (sort-by first (label (into {} (map (fn [[a i]] [a (vec i)]) new-cands))))))
