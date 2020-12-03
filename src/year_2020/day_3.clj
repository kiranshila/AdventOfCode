(ns year-2020.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "2020/3/input")))

(defn toboggan [input [dy dx]]
  (->> (for [i (range 0 (count input) dx)
             :let [line (get input i)
                   j (/ (* i dy) dx)]]
         (nth (cycle line) j))
       (filter #(= \# %))
       count))

(def movement [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn solution-1 [input]
  (toboggan (str/split-lines input) [3 1]))

(defn solution-2 [input]
  (apply * (for [move movement]
             (toboggan (str/split-lines input) move))))
