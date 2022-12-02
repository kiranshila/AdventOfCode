(ns year-2020.day-24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp (io/resource "2020/24/input")))

(defn parse-identifiers [input]
  (->> (re-seq #"(e|se|sw|w|nw|ne)" input)
       (map first)
       (map keyword)
       (into [])))

(defn adjacent [[x y] instruction]
  (case instruction
    :e  [(inc x) (inc y)]
    :se [(inc x) y]
    :sw [x (dec y)]
    :w  [(dec x) (dec y)]
    :nw [(dec x) y]
    :ne [x (inc y)]))

(defn identify [instructions]
  (reduce adjacent [0 0] instructions))

(defn black-tiles [input]
  (->> (str/split-lines input)
       (map parse-identifiers)
       (map identify)
       frequencies
       (filter (fn [[k v]] (odd? v)))
       keys
       (into #{})))

(defn solution-1 [input]
  (count (black-tiles input)))

(defn adjacents [tile]
  (map (partial adjacent tile) [:e :se :sw :w :nw :ne]))

(defn black? [tiles tile]
  (contains? tiles tile))

(defn white? [tiles tile]
  (not (black? tiles tile)))

(defn alive? [tile tiles]
  (let [adjs (->> tile
                  adjacents
                  (keep tiles)
                  count)]
    (or (and (black? tiles tile)
             (or (= adjs 1)
                 (= adjs 2)))
        (and (white? tiles tile)
             (= adjs 2)))))

(defn evolve [tiles]
  (let [to-check (->> tiles
                      (map #(into #{} (adjacents %)))
                      (apply set/union tiles))]
    (into #{}
          (for [tile to-check
                :when (alive? tile tiles)]
            tile))))

(defn solution-2 [input]
  (->> input
       black-tiles
       (iterate evolve)
       (#(nth % 100))
       count))
