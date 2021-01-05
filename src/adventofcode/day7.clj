(ns adventofcode.day7)

(def input7 (clojure.string/split-lines (slurp "input7")))

(defn read-n-and-colors [line]
  (loop [[n color1 color2 _ & t :as x] line
         acc [] ]
    (if (seq x)
      (if (= "no" n)
        nil
        (recur
          t
          (conj acc [(read-string n) (str color1 color2)])))
      acc)))

(defn extract-rules [line]
  (let [[bagtype11 bagtype12 _ _ & t] (clojure.string/split line #" ")]
    [(str bagtype11 bagtype12) (read-n-and-colors t)]))


(def rules (into {} (map extract-rules input7)))

(defn expand-bag [all-rules [n bagtype]]
  (map (fn [[m color]] [(* n m) color]) (get all-rules bagtype)))

(defn expand-all-bags [all-rules bags]
  (mapcat (partial expand-bag all-rules) bags)) 

(expand-all-bags rules [[1 "shinytomato"]])

(defn deep-expand [rules bag]
  (loop [seen [[1 bag]]
         current [[1 bag]]]
    (let [next-step (expand-all-bags rules current)]
      (if (seq current)
        (recur (into seen next-step)
               next-step)
        [bag seen]))))

(def encounters (into {} (map (partial deep-expand rules) (keys rules))))

(reduce + (map first (get encounters "shinygold")))
(count (keys (into {} (filter (fn [[k v]] (get v "shinygold")) encounters))))
