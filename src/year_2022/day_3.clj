(ns year-2022.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn priority [c]
  (if (Character/isUpperCase c)
    (- (int c) 38)
    (- (int c) 96)))

(->> (slurp (io/resource "2022/3/input"))
     (str/split-lines)
     (map #(let [size (/ (count %) 2)
                 a (set (take size %))
                 b (set (take-last size %))]
             (set/intersection a b)))
     (map first)
     (map priority)
     (reduce +))

(->> (slurp (io/resource "2022/3/input"))
     (str/split-lines)
     (partition 3)
     (map (fn [[a b c]]
            (set/intersection
             (set a)
             (set b)
             (set c))))
     (map first)
     (map priority)
     (reduce +))
