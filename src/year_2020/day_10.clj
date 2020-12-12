(ns year-2020.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (slurp (io/resource "2020/10/input")))

(defn get-chain [input]
  (->> input
       str/split-lines
       (map edn/read-string)
       sort
       (into [0])
       (#(conj % (+ 3 (apply max %))))))

(def tribonacci
  ((fn trib [a b c]
     (lazy-seq (cons a (trib b c (+ a b c)))))
   1 1 2))

(defn derivative [chain]
  (for [i (range 0 (dec (count chain)))]
    (- (nth chain (inc i)) (nth chain i))))

(defn solution-1 [input]
  (let [freq (->> input
                  get-chain
                  derivative
                  frequencies)]
    (* (freq 1) (freq 3))))

(defn solution-2 [input]
  (->> input
       get-chain
       derivative
       (partition-by #{3})
       (filter #(every? (partial = 1) %))
       (map count)
       (map #(nth tribonacci %))
       (reduce *)))
