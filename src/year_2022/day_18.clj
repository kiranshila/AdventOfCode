(ns year-2022.day-18
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [progrock.core :as pr]))

(def input (slurp (io/resource "2022/18/input")))

(def neighbors
  [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn parse-line [line]
  (let [coords (rest (first (re-seq #"(\d+),(\d+),(\d+)" line)))]
    (mapv parse-long coords)))

(defn cubes [input]
  (->> (str/split-lines input)
       (mapv parse-line)
       (into #{})))

(defn total-surface-area [cubes]
  (reduce + (for [cube cubes
                  neighbor neighbors]
              (if (contains? cubes (mapv + neighbor cube))
                0
                1))))

(def cube-bounds
  (memoize
   (fn  [cubes]
     (let [xs (map #(nth % 0) cubes)
           ys (map #(nth % 1) cubes)
           zs (map #(nth % 2) cubes)
           minx (apply min xs) maxx (apply max xs)
           miny (apply min ys) maxy (apply max ys)
           minz (apply min zs) maxz (apply max zs)]
       [[minx maxx] [miny maxy] [minz maxz]]))))

(defn touches-air? [[x y z] cubes]
  (let [[[minx maxx] [miny maxy] [minz maxz]] (cube-bounds cubes)]
    (or (= (dec minx) x) (= (inc maxx) x)
        (= (dec miny) y) (= (inc maxy) y)
        (= (dec minz) z) (= (inc maxz) z))))

(def is-interior?
  (memoize
   (fn  [cube cubes]
     (loop [explored #{cube}
            to-check (conj (clojure.lang.PersistentQueue/EMPTY) cube)
            touches-air false]
       (if (or touches-air (empty? to-check))
         (not touches-air)
         (let [next-cube (peek to-check)
               maybe-check (for [neighbor neighbors] (mapv + neighbor next-cube))
               touches-air (some? (some #(touches-air? % cubes) maybe-check))
               filtered-check (filter #(and (not (contains? explored %))
                                            (not (contains? cubes %)))
                                      maybe-check)]
           (recur (apply conj explored filtered-check)
                  (apply conj (pop to-check) filtered-check)
                  touches-air)))))))

(defn outer-surface-area [cubes]
  (reduce + (for [[idx cube] (map-indexed vector cubes)
                  neighbor neighbors
                  :let [neighbor (mapv + neighbor cube)]
                  :when (not (is-interior? neighbor cubes))]
              (if (contains? cubes neighbor)
                0
                1))))
