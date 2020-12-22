(ns year-2020.day-18
  (:require [clojure.java.io :as io]
            [incanter.core :refer-macros [$= defop]]
            [clojure.string :as str]))

(def input (slurp (io/resource "2020/18/input")))

(defop '+ 100)
(defop '* 60) ; Change me to 100 for part 1

(defn solution [input]
  (->> (str/split-lines input)
       (map #(str \( % \)))
       (map read-string)
       (map #(eval `($= ~%)))
       (reduce +)))
