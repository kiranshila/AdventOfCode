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
     (map (fn [line]
            (let [size (/ (count line) 2)
                  a (into #{} (take size line))
                  b (into #{} (take-last size line))]
              (set/intersection a b))))
     (map first)
     (map priority)
     (reduce +))

(->> (slurp (io/resource "2022/3/input"))
     (str/split-lines)
     (partition 3)
     (map (fn [[a b c]]
            (set/intersection
             (into #{} (seq a))
             (into #{} (seq b))
             (into #{} (seq c)))))
     (map first)
     (map priority)
     (reduce +))
