(ns year-2022.day-6
  (:require [clojure.java.io :as io]))

(let [n 14]
  (->> (seq (slurp (io/resource "2022/6/input")))
       (partition n 1)
       (map #(= (count %) (count (set %))))
       (#(+ n (.indexOf % true)))))
