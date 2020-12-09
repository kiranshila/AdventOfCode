(ns year-2020.day-9
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def input (slurp (io/resource "2020/9/input")))

(defn check-sum? [list sum]
  (let [combinations (into [] (combo/combinations list 2))]
    (contains? (into #{} (map #(reduce + %) combinations)) sum)))

(defn solution-1 [input preamble-size]
  (first (let [nums (->> input
                         str/split-lines
                         (map edn/read-string)
                         (into []))]
           (for [i (range preamble-size (count nums))
                 :when (not (check-sum? (subvec nums (- i preamble-size) i) (nth nums i)))]
             (nth nums i)))))

(defn solution-2 [input preamble-size]
  (first (let [nums (->> input
                         str/split-lines
                         (map edn/read-string)
                         (into []))
               target (solution-1 input preamble-size)]
           (for [i (range (count nums))
                 j (range (inc i) (count nums))
                 :let [sub (subvec nums i j)
                       sub-sum (reduce + sub)]
                 :when (= sub-sum target)]
             (+ (apply min sub) (apply max sub))))))
