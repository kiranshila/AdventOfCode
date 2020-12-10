(ns year-2020.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (slurp (io/resource "2020/10/input")))

(defn get-chain [input]
  (->> input
       str/split-lines
       (map edn/read-string)
       (into [])
       sort
       (into [0])
       (#(conj % (+ 3 (apply max %))))))

(def tribonacci
  ((fn trib [a b c]
     (lazy-seq (cons a (trib b c (+ a b c)))))
   0 1 1))

(defn derivative [chain]
  (into [] (for [i (range 0 (dec (count chain)))]
             (- (nth chain (inc i)) (nth chain i)))))

(defn solution-1 [input]
  (let [chain (get-chain input)
        deriv (derivative chain)
        freq (frequencies deriv)]
    (* (freq 1) (freq 3))))

(defn solution-2 [input]
  (->> input
       get-chain
       derivative
       (apply str)
       (#(str/split % #"3")) ;this is silly, but I couldn't think of the nice vector way
       (filter #(not (str/blank? %)))
       (map count)
       (map #(nth tribonacci (inc %)))
       (reduce *)))
