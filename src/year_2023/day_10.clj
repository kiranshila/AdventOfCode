(ns year-2023.day-10
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.math :as math]))

(def input (slurp (io/resource "2023/10/input")))
(def example (slurp (io/resource "2023/10/example")))

(def delta
  {\N [-1 0] \S [1 0] \E [0 1] \W [0 -1]})

(def valid
  {\N #{\F \7 \|} \S #{\L \J \|} \E #{\J \7 \-} \W #{\F \L \-}})

(def connection
  {\| #{\N \S} \- #{\E \W} \L #{\N \E} \J #{\N \W} \7 #{\S \W} \F #{\S \E}})

(defn parse-pipes [input]
  (->> (str/split-lines input)
       (map-indexed vector)
       (map (fn [[i row]]
              (for [[j c] (map-indexed vector row)
                    :when (not= c \.)]
                [[i j] c])))
       (filter seq)
       (map #(into {} %))
       (apply merge)))

(defn v+ [v vs]
  (map (partial mapv + v) vs))

(defn next-valid [pipe-map visited pos]
  (->> (pipe-map pos)
       connection
       (map delta)
       (v+ pos)
       (into #{})
       (#(set/difference % visited))
       first))

(defn next-start [pipe-map start]
  (first
   (for [[delta valid] (map (juxt delta valid) [\N \S \E \W])
         :let [pos (mapv + start delta)
               c (pipe-map pos)]
         :when (valid c)]
     pos)))

(defn trace-path [pipe-map]
  (let [start (first (keep (fn [[k v]] (when (= \S v) k)) pipe-map))]
    (loop [visited #{start}
           current (next-start pipe-map start)]
      (if-let [next (next-valid pipe-map visited current)]
        (recur (conj visited current) next)
        visited))))

(defn part-one [input]
  (->> (parse-pipes input)
       trace-path
       count
       (#(quot % 2))
       inc))
