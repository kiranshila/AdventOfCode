(ns year-2020.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "2020/1/input"))
                str/split-lines
                (map #(Integer/parseInt %))
                (into [])))

(defn solution-1 [input num]
  (->> (for [i (range (count input))]
         (let [this (get input i)
               remainder (- num this)]
           (for [rest (nthrest input (inc i))]
             (when (== rest remainder)
               (* rest this)))))
       flatten
       (filter identity)
       first))

(defn solution-2 [input num]
  (->> (for [i (range (count input))]
         (let [this (get input i)
               remainder (- num this)]
           (let [rest (into [] (nthrest input (inc i)))]
             (when-let [found (solution-1 rest remainder)]
               (* found this)))))
       flatten
       (filter identity)
       first))
