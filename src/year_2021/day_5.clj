(ns year-2021.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "2021/5/input"))
                str/split-lines))

(defn parse-line-segment [line]
  (->> (re-seq #"(\d+),(\d+) -> (\d+),(\d+)" line)
       first
       rest
       (map #(Long/parseLong %))
       (into [])))

(defn true-range [p1 p2 & [distance]]
  (if (= p1 p2)
    (take distance (repeat p1))
    (if (< p1 p2)
      (range p1 (inc p2))
      (range p1 (dec p2) -1))))

(defn trace [[x1 y1 x2 y2]]
  (let [distance (inc (max (Math/abs (- y2 y1)) (Math/abs (- x2 x1))))
        range-x (true-range x1 x2 distance)
        range-y (true-range y1 y2 distance)]
    (map vector range-x range-y)))

(defn points [segments]
  (->> (map trace segments)
       (filter identity)
       flatten
       (partition 2)
       frequencies))

(defn solve [input]
  (->> (map parse-line-segment input)
       #_(filter #(let [[x1 y1 x2 y2] %]
                    (or (= x1 x2)
                        (= y1 y2))))
       points
       (filter #(>= (second %) 2))
       count))

(solve input)
