(ns adventofcode.day1
  (:require [adventofcode.commons :refer [read-file]]))

(let [xx (read-file "input1")]
    (for [a xx b xx
          :when (= 2020 (+ a b)) ]
      (* a b)))

(let [xx (read-file "input1")]
    (for [a xx b xx c xx
          :when (= 2020 (+ a b c)) ]
      (* a b c)))
