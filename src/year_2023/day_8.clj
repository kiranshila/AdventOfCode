(ns year-2023.day-8
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.numeric-tower :refer [lcm]]))

(def input (slurp (io/resource "2023/8/input")))
(def example (slurp (io/resource "2023/8/example")))

(defn parse-line [line]
  (let [[_ s l r] (first (re-seq #"(\S+)\s+=\s+\((\S+),\s+(\S+)\)" line))]
    [s [l r]]))

(defn parse-input [input]
  (let [[dirs m] (str/split input #"\n\n")]
    [(seq dirs)
     (->> (str/split-lines m)
          (map parse-line)
          (into {}))]))

(defn move [tree dir pos]
  (({\L first \R second} dir) (tree pos)))

(defn step-to-end [dirs tree start end-pred]
  (loop [loc start
         steps 0
         dirs (cycle dirs)]
    (if (end-pred loc)
      steps
      (recur (move tree (first dirs) loc)
             (inc steps)
             (rest dirs)))))

(defn part-one [input]
  (let [[dirs tree] (parse-input input)]
    (step-to-end dirs tree "AAA" #(= "ZZZ" %))))

(defn find-starts [tree]
  (filter #(= \A (last %)) (keys tree)))

(defn part-two [input]
  (let [[dirs tree] (parse-input input)
        starts (find-starts tree)
        ds (map (fn [s] (step-to-end dirs tree s #(= \Z (last %)))) starts)]
    (reduce lcm ds)))
