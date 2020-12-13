(ns year-2020.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.linear :as lin]))

(def input (slurp (io/resource "2020/12/input")))

(defn parse-lines [input]
  (for [line (str/split-lines input)]
    (let [[dir arg] (rest (first (re-seq #"(\w)(\d+)" line)))]
     [(keyword dir) (edn/read-string arg)])))

(defn rotate [dir [x y]]
  (case dir
    :R [y (- x)]
    :L [(- y) x]))

(def direction {0 :N, 90 :E, 180 :S, 270 :W})

(defmulti move
  "Takes a [ship facing] and [action arg], returns a new [ship facing]"
  (fn [_ [action _]] action))

(defmethod move :N [[ship facing] [_ arg]] [(mat/add ship [0 arg]) facing])
(defmethod move :S [[ship facing] [_ arg]] [(mat/add ship [0 (- arg)]) facing])
(defmethod move :E [[ship facing] [_ arg]] [(mat/add ship [arg 0]) facing])
(defmethod move :W [[ship facing] [_ arg]] [(mat/add ship [(- arg) 0]) facing])
(defmethod move :L [[ship facing] [_ arg]] [ship (mod (- facing arg) 360)])
(defmethod move :R [[ship facing] [_ arg]] [ship (mod (+ facing arg) 360)])
(defmethod move :F [[ship facing] [_ arg]] (move [ship facing] [(direction facing) arg]))

(defmulti move-2
  "Takes a [waypoint ship] and [action arg], returns a new [waypoint ship]"
  (fn [_ [action _]] action))

(defmethod move-2 :L [[waypoint ship] [_ arg]]
  [(mat/add (nth (iterate #(rotate :L %) waypoint) (/ arg 90))) ship])
(defmethod move-2 :R [[waypoint ship] [_ arg]]
  [(mat/add (nth (iterate #(rotate :R %) waypoint) (/ arg 90))) ship])
(defmethod move-2 :F [[waypoint ship] [_ arg]]
  [waypoint (mat/add ship (mat/mul arg waypoint))])
(defmethod move-2 :default [w-s command] (move [w-s command]))

(defn solution-1 [input]
  (->> (parse-lines input)
       (reduce move [[0 0] 90])
       first
       (#(lin/norm % 1))))

(defn solution-2 [input]
  (->> (parse-lines input)
       (reduce move-2 [[10 1] [0 0]])
       second
       (#(lin/norm % 1))))
