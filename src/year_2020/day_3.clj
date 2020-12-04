(ns year-2020.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "2020/3/input")))

(def tree? (partial = \#))

(defn toboggan [input [down right]]
  (->> (for [i (range 0 (count input) down)
             :let [line (get input i)
                   j (/ (* i right) down)]]
         (nth (cycle line) j))
       (map tree?)
       (filter true?)
       count))

(def movement [[1 1] [1 3] [1 5] [1 7] [2 1]])

(defn solution-1 [input]
  (toboggan (str/split-lines input) [1 3]))

(defn solution-2 [input]
  (apply * (for [move movement]
             (toboggan (str/split-lines input) move))))
