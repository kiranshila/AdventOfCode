(ns year-2020.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "2020/1/input"))
                str/split-lines
                (map #(Integer/parseInt %))
                (into [])))

(defn solution-1 [input num]
  (first (for [i (range (count input))
               :let [this (get input i)
                     remainder (- num this)]
               rest (nthrest input (inc i))
               :when (== rest remainder)]
           (* rest this))))

(defn solution-2 [input num]
  (first (for [i (range (count input))
               :let [this (get input i)
                     remainder (- num this)
                     rest (into [] (nthrest input (inc i)))
                     found (solution-1 rest remainder)]
               :when found]
           (* found this))))
