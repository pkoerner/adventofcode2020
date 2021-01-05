(ns adventofcode.day20
  (:require [clojure.string]))

(def input20 (clojure.string/split-lines (slurp "input20")))
(def tiles (loop [c input20
                  acc {}
                  cur nil]
             (cond (empty? c) acc
                   (empty? (first c)) (recur (rest c) acc nil)
                   (nil? cur) (let [n (read-string (apply str (take 4 (drop 5 (first c)))))] (recur (rest c) (assoc acc n []) n)) 
                   :otherwise (recur (rest c) (update acc cur conj (first c)) cur))))

(defn flip-vert [tile] (reverse tile))
(defn flip-hori [tile] (map (comp (partial apply str) reverse) tile))
(defn rotate1 [tile] (map (comp (partial apply str) reverse) (apply map vector tile)))

; generate all tile variations
(defn candidates [tile] (into #{} (concat #_[(flip-vert tile) (flip-hori tile) (flip-vert (flip-hori tile))] 
                                          (take 4 (iterate rotate1 (flip-hori (flip-vert tile))))  
                                          (take 4 (iterate rotate1 (flip-hori tile)))  
                                          (take 4 (iterate rotate1 (flip-vert tile)))  
                                          (take 4 (iterate rotate1 tile)))))

(defn neighbours [[x y]]
  ;; only top and left?
  (map  (fn [[x' y']] (when (and (<= 0 y' 12) (<= 0 x' 12)) [x' y']))
          [#_[(inc x) y] [(dec x) y] #_[x (inc y)] [x (dec y)]]))

(defn next-pos [[x y]]
  (cond (< x (dec 12)) [(inc x) y]
        (< y (dec 12)) [0 (inc y)]
        :otherwise nil))

(def all-tiles (for [[id tile] tiles
                     c (candidates tile)]
                 {:id id :tile c}))

(defn label []
  (let [grid (vec (repeat 12 (vec (repeat 12 nil))))
        pos [0 0]] 
    ;; this is a backtracking algorithm inspired by Prolog.
    ;; if there are multiple possibilities (solutions),
    ;; the current program state is saved and restored
    ;; if no solution can be found later on
    (loop [grid grid
           pos pos
           tiles-available all-tiles
           tileys all-tiles
           choices ()]
      (if pos
        (if (seq tiles-available)
          (let [choice (first tiles-available)
                [n1 n2] (neighbours pos)]
            (if
              ;; tile matches?
              (and (or (not n1) (= (map first (:tile choice)) (map last (:tile (get-in grid (reverse n1))))))
                   (or (not n2) (= (first (:tile choice)) (last (:tile (get-in grid (reverse n2)))))))
              (recur (assoc-in grid (reverse pos) choice)
                     (next-pos pos)
                     (remove #(= (:id %) (:id choice)) tileys) 
                     (remove #(= (:id %) (:id choice)) tileys) 
                     (cons [grid pos tiles-available tileys] choices))
              (recur grid pos (rest tiles-available) tileys choices)))
          (let [[[grid pos tiles tileys] & t] choices]
            (recur grid pos (rest tiles) tileys t)))
        grid))))


(let [y (first xx)
      z (last xx)]
  (* (:id (first y)) (:id (last y)) (:id (first z)) (:id (last z))))

(def xx (label))

(defn update-tile [tile]
  (map (fn [s] (apply str (rest (butlast s)))) (rest (butlast tile))))

(def image-tile
  (apply concat (for [tile-line xx]
                  (apply map str (map (comp update-tile :tile) tile-line)))))

;                  #
;#    ##    ##    ###
; #  #  #  #  #  #


;; HACK: hardcode a seamonster in the picture 
;; by adding a sufficient amount of water
;; and ignoring linebreak entirely
(def seamonster (str "..................#." (apply str (repeat (- 96 20) \.))
                     "#....##....##....###" (apply str (repeat (- 96 20) \.))
                     ".#..#..#..#..#..#..."))

(defn replace-seamonster [s]
  (loop [s s
         pattern seamonster
         acc []]
    (if (seq pattern)
      (if (= (first pattern) \#)
        (recur (rest s) (rest pattern) (conj acc \.))
        (recur (rest s) (rest pattern) (conj acc (first s)))
        )
      (apply str (concat acc s)))))

(defn find-seamonster? [s]
  (loop [s s
         pattern seamonster]
    (if (seq pattern)
      (when (or (= (first pattern) \.)
                (= (first s) \#))
        (recur (rest s) (rest pattern)))
      true)))

(defn replace-all-seamonsters
  [image-str]
  (loop [s image-str
         acc []]
    (if (seq s)
      (if (find-seamonster? s)
        (recur (replace-seamonster s) acc)
        (recur (rest s) (conj acc (first s))))
      acc)))

;; manually found the correct orientation of the picture
(def image-str (apply str (-> image-tile
;                              flip-hori
                              flip-vert
;                              rotate1
;                              rotate1
;                              rotate1
                              )))

(frequencies (apply str (replace-all-seamonsters image-str)))

