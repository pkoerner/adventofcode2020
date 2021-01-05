(ns adventofcode.day4
  (:require [adventofcode.commons :refer [read-groups]]
            [clojure.string]))

(def passports (read-groups "input4"))

(defn handle-passport [passport]
  (let [strs (mapcat (fn [x] (clojure.string/split x #" ")) passport)]
    (into {} (map (fn [s] (clojure.string/split s #":")) strs))))

(def valid-passports1 
  (filter 
    (fn [passport] (empty? (clojure.set/difference #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}
                                                   (set (keys passport)))))
    (map handle-passport passports)))


;; TODO: use clojure.spec instead
(defn validate-passport [{:strs [byr iyr eyr hgt hcl ecl pid]}]
  (let [ztn #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}]
    (and (<= 1920 (read-string byr) 2002)
         (<= 2010 (read-string iyr) 2020)
         (<= 2020 (read-string eyr) 2030)
         (let [unit (apply str (take-last 2 hgt))]
           (cond
             (= unit "cm") (<= 150 (read-string (apply str (drop-last 2 hgt))) 193)
             (= unit "in") (<= 59 (read-string (apply str (drop-last 2 hgt))) 76)
             :otherwise nil))
         ;; hgt
         (= \# (first hcl))
         (= (seq (rest hcl)) (filter (into ztn #{\a \b \c \d \e \f}) (rest hcl)))
         (= 6 (count (rest hcl)))
         (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
         (= (seq pid) (filter ztn pid))
         (= 9 (count pid)))))

(count (filter validate-passport valid-passports1))
