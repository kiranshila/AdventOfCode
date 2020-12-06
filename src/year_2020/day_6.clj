(ns year-2020.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp (io/resource "2020/6/input")))

(defn solution-1 [input]
  (let [groups (map set (str/split input #"\n\n"))
        groups (map #(disj % \newline) groups)]
    (apply + (map count groups))))

(defn solution-2 [input]
  (let [groups (str/split input #"\n\n")
        people (map #(str/split % #"\n") groups)]
    (apply + (for [person people]
       (count (apply set/intersection (map set person)))))))
