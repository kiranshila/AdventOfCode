(ns year-2021.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(->> (slurp (io/resource "2021/1/input"))
     str/split-lines
     (map #(Integer/parseInt %))
     ; Part 2
     #_(partition 3 1)
     #_(map #(apply + %))
     (partition 2 1)
     (map #(< (first %) (second %)))
     (filter identity)
     count)
