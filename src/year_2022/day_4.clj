(ns year-2022.day-4
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn parse [pair]
  (let [[s e] (map parse-long (str/split pair #"-"))]
    (into #{} (range s (inc e)))))

(defn pairwise-subset [a b]
  (or (set/subset? a b)
      (set/subset? b a)))

(defn pairwise-intersect [a b]
  (seq (set/intersection a b)))

(->> (slurp (io/resource "2022/4/input"))
     str/split-lines
     (map #(str/split % #","))
     (map #(map parse %))
     #_(map #(apply pairwise-subset %))
     (map #(apply pairwise-intersect %))
     (filter identity)
     count)
