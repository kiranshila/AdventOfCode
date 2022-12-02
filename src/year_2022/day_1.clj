(ns year-2022.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(->> (str/split
      (slurp (io/resource "2022/1/input")) #"\n\n")
     (map str/split-lines)
     (map #(map parse-long %))
     (map #(reduce + %))
     (sort >)
     first
     #_(take 3)
     #_(reduce +))
