(ns year-2017.day-1
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def input (slurp (io/resource "2017/1/input")))

(defn solution-1 [input]
  (let [digits (->> (str/trim input)
                (seq)
                    (map str)
                    (map #(Integer/parseInt %)))]

    (->> digits
         (partition 2 1 digits)
         (filter #(= (first %) (second %)))
         (map first)
         (reduce +))))

(defn group [col]
  (let [n (count col)
        loop-col (cycle col)]
    (for [i (range n)]
      (list (nth loop-col i) (nth loop-col (+ i (/ n 2)))))))

(defn solution-2 [input]
  (let [digits (->> (str/trim input)
                (seq)
                    (map str)
                    (map #(Integer/parseInt %)))
        n (count digits)]

    (->> digits
         group
         (filter #(= (first %) (second %)))
         (map first)
         (reduce +))))

(solution-1 input)
(solution-2 input)
