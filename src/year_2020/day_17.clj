(ns year-2020.day-17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(def input (slurp (io/resource "2020/17/input")))

(defn get-neighbor-points
  "Given a point, returns a seq of neighbor points"
  [point]
  (->> point
       (map #(vector (dec %) % (inc %)))
       (apply combo/cartesian-product)
       (map vec)
       (into #{})
       (#(set/difference % #{point}))))

(defn update-cube
  "Impl of conway rules, given a point - return true or nil"
  [cubes cube]
  (let [active-neighbors (->> (get-neighbor-points cube)
                              (keep cubes)
                              count)]
    (if (cubes cube)
      (<= 2 active-neighbors 3)
      (= 3 active-neighbors))))

(defn create-cubes [input dim]
  (let [lines (str/split-lines input)]
    (apply set/union (for [y (range (count lines))
                           x (range (count (first lines)))
                           :when (= \# (nth (nth lines y) x))]
                       #{(apply conj [x y] (repeat (- dim 2) 0))}))))

(defn solution [input dim]
  (loop [cubes (create-cubes input dim)
         iter 0]
    (if (= iter 6)
      (count cubes)
      (recur
       (->> cubes
            (map get-neighbor-points)
            (apply set/union cubes)
            (filter (partial update-cube cubes))
            set)
       (inc iter)))))
