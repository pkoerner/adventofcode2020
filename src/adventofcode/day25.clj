(ns adventofcode.day25)

(def pub1 11404017)
(def pub2 13768789)

(defn hack-loopsize [n]
  (loop [i 0
         v 1]
    (if (= v n)
      i
      (recur (inc i) (mod (* v 7) 20201227)))))

(def loopsize1 (hack-loopsize pub1))
(def loopsize2 (hack-loopsize pub2))

(defn transform [m n]
  (loop [i 0
         v 1]
    (if (= i n)
      v
      (recur (inc i) (mod (* v m) 20201227)))))

(def enc1 (transform pub2 loopsize1))
(def enc2 (transform pub1 loopsize2))
[enc1 enc2]
(transform 17807724 8)
