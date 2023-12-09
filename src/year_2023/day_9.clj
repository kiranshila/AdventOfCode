(ns year-2023.day-9
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (slurp (io/resource "2023/9/input")))
(def example (slurp (io/resource "2023/9/example")))

(defn finite-difference [nums]
  (map #(apply - (reverse %)) (partition 2 1 nums)))

(defn ex-f [nums]
  (loop [ends [(last nums)]
         nums nums]
    (if (every? (partial = 0) nums)
      (reduce + ends)
      (let [ds (finite-difference nums)]
        (recur (conj ends (last ds)) ds)))))

(defn ex-b [nums]
  (loop [starts [(first nums)]
         nums nums]
    (if (every? (partial = 0) nums)
      (reduce #(- %2 %1) (reverse starts))
      (let [ds (finite-difference nums)]
        (recur (conj starts (first ds)) ds)))))

(defn solve [input ex-fn]
  (->> (str/split-lines input)
       (map #(str/split % #"\s+"))
       (map #(mapv parse-long %))
       (map ex-fn)
       (reduce +)))

;; Part 1
(solve input ex-b)
;; Part 2
(solve input ex-b)
