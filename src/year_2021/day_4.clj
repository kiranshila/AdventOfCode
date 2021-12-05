(ns year-2021.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def remaining (->> (slurp (io/resource "2021/4/input"))
                    str/split-lines
                    first
                    (#(str/split % #","))
                    (map #(Long/parseLong %))))

(defn parse-board [lines]
  (into []
        (for [row (range 5)]
          (->> (nth lines row)
               (#(str/split % #" "))
               (filter seq)
               (map #(Long/parseLong %))
               (into [])))))

(def boards (->> (slurp (io/resource "2021/4/input"))
                 str/split-lines
                 rest
                 (filter seq)
                 (partition 5)
                 (map parse-board)))

(defn solved? [board called]
  (->> (for [row (range 5)]
         [(seq (set/difference (set (nth board row)) called))
          (seq (set/difference (set (nth (apply mapv vector board) row)) called))])
       flatten
       (filter nil?)
       count
       pos?))

(defn score [board called]
  (->> (flatten board)
       (into #{})
       (#(set/difference % (set called)))
       (reduce +)
       (* (first called))))

(defn part1
  ([boards remaining] (part1 boards (take 5 remaining) (nthrest remaining 5)))
  ([boards called remaining]
   (let [solved-board (first (filter #(solved? % (set called)) boards))]
     (if solved-board
       (score solved-board called)
       (recur boards (conj called (first remaining)) (rest remaining))))))

(part1 boards remaining)

; ill defined problem, the fact that multiple can be solved for a given number, but not the last one is wack
(defn part2
  ([boards remaining] (part2 boards (take 5 remaining) (nthrest remaining 5) []))
  ([boards called remaining solved-idxs]
   (if (= (count boards) (count solved-idxs))
     (score (nth boards (last solved-idxs)) (rest called))
     (let [next-solved-idxs (for [i (set/difference (set (range (count boards))) (set solved-idxs))
                                  :let [board (nth boards i)]
                                  :when (solved? board (set called))]
                              i)]
       (if (seq next-solved-idxs)
         (recur boards (conj called (first remaining)) (rest remaining) (into solved-idxs next-solved-idxs))
         (recur boards (conj called (first remaining)) (rest remaining) solved-idxs))))))

(part2 boards remaining)
