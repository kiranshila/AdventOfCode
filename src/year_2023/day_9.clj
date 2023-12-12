(ns year-2023.day-9
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (slurp (io/resource "2023/9/input")))
(def example (slurp (io/resource "2023/9/example")))

(defn finite-difference [nums]
  (map #(apply - (reverse %)) (partition 2 1 nums)))

(defn extrap [nums]
  (loop [ends [(last nums)]
         nums nums]
    (if (every? zero? nums)
      (reduce + ends)
      (let [ds (finite-difference nums)]
        (recur (conj ends (last ds)) ds)))))

(defn solve [input & {:keys [rev]}]
  (cond->> (str/split-lines input)
       true (map #(str/split % #"\s+"))
       true (map #(mapv parse-long %))
       rev (map reverse)
       true (map extrap)
       true (reduce +)))

(solve input :rev true)
