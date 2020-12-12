(ns year-2020.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.linear :as lin]))

(def input (slurp (io/resource "2020/12/input")))

(defn parse-line [str]
  (let [[dir arg] (rest (first (re-seq #"(\w)(\d+)" str)))
        dir (keyword dir)
        arg (edn/read-string arg)]
    [dir arg]))

(defn move [[position facing] [action arg]]
  (case action
    :N [(mat/add position [0 arg]) facing]
    :S [(mat/add position [0 (- arg)]) facing]
    :E [(mat/add position [arg 0]) facing]
    :W [(mat/add position [(- arg) 0]) facing]
    :L [position (mod (- facing arg) 360)]
    :R [position (mod (+ facing arg) 360)]
    :F (let [e (* arg (Math/sin (Math/toRadians facing)))
             n (* arg (Math/cos (Math/toRadians facing)))]
         [(mat/add position [e n]) facing])))

(defn rotate [dir [x y]]
  (case dir
    :R [y (- x)]
    :L [(- y) x]))

(defn move-2 [[waypoint ship] [action arg]]
  (case action
    :N [(mat/add waypoint [0 arg]) ship]
    :S [(mat/add waypoint [0 (- arg)]) ship]
    :E [(mat/add waypoint [arg 0]) ship]
    :W [(mat/add waypoint [(- arg) 0]) ship]
    :L [(mat/add (nth (iterate #(rotate :L %) waypoint) (/ arg 90))) ship]
    :R [(mat/add (nth (iterate #(rotate :R %) waypoint) (/ arg 90))) ship]
    :F [waypoint (mat/add ship (mat/mul arg waypoint))]))

(defn solution-1 [input]
  (let [commands (->> input
                   str/split-lines
                   (map parse-line))]
    (loop [commands commands
           ship [[0 0] 90]]
      (if (empty? commands)
        (lin/norm (first ship) 1)
        (recur (rest commands) (move ship (first commands)))))))

(defn solution-2 [input]
  (let [commands (->> input
                      str/split-lines
                      (map parse-line))]
    (loop [commands commands
           w-s [[10 1] [0 0]]]
      (if (empty? commands)
        (lin/norm (second w-s) 1)
        (recur (rest commands) (move-2 w-s (first commands)))))))
