(ns adventofcode.day6
  (:require [adventofcode.commons :refer [read-groups]]))

(def forms (read-groups "input6"))

(defn count-group [group]
  (count (set (apply concat group))))

(reduce + (map count-group forms))

(defn count-group2 [group]
  (let [all-answers (set (apply concat group))]
    (count (filter (fn [c] (every? (fn [form] (get (set form) c))
                                   group))
                   all-answers))))

(reduce + (map count-group2 forms))

