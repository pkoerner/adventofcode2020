(ns adventofcode.day13
  (:require [clojure.string]))

(def input13 (slurp "input13"))
(def infos (let [[timestamp lines] (clojure.string/split-lines input13)
                 bus-lines (map read-string(remove #{"x"} (clojure.string/split lines #",")))]
             [(read-string timestamp) bus-lines]))

(defn calculate-waiting-time-and-line [[estimate infos]]
  (map first
       (map (fn [multiples] (drop-while (fn [m] (< m estimate)) multiples))
            (map (fn [x] (iterate (fn [v] (+ v x)) 0)) infos))))

(def line-and-time (first (sort-by second (zipmap (second infos) (calculate-waiting-time-and-line infos)))))
(* (first line-and-time) (- (second line-and-time) (first infos)))

(def infos2 (let [[timestamp lines] (clojure.string/split-lines input13)
                 bus-lines (map (fn [[offset line]] [offset (read-string line)]) (remove (comp #{"x"} second) (map-indexed vector (clojure.string/split lines #","))))]
              bus-lines))

infos2
[0 17] [7 41] [17 523] [35 13] [36 19] [40 23] [48 787] [54 37] [77 29]

;; euklid function from the cryptocomplexity lecture I attended
(defn euklid [n m]
  (if (= m 0)
    (list [1 0 n 1 0])
    (let [r (euklid m (mod n m))
          [_ _ g x' y'] (first r)
          x y'
          y (- x' (* y' (quot n m)))
          ]
      (conj r [n m g x y]))))

(euklid 4 15)

;; and thank goodness Prof. Rothe taught me the chinese reminder theorem!

(def crt-input (map (fn [[x y]] [(- x) y]) infos2))
(def M (reduce *' (map second crt-input)))
(def M_x (map (fn [[_ y]] (/ M y)) crt-input))
(def euklid-inputs (map (fn [[x y] M_i] [y M_i]) crt-input M_x))
(def factors (map (fn [input] (let [[[_ s _ _ t]] (apply euklid input)] (* s t))) euklid-inputs))
(mod (apply + (map * factors (map first crt-input))) M)


