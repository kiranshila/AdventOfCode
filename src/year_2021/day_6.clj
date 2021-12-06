(ns year-2021.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "2021/6/input"))
                str/trim
                (#(str/split % #","))
                (map #(Long/parseLong %))
                frequencies))

(defn step [timers]
  (into {}
        (for [i (range 9)]
          (case i
            6 [6 (+ (timers 0 0) (timers 7 0))]
            8 [8 (timers 0 0)]
            [i (timers (inc i) 0)]))))

(reduce + (vals (nth (iterate step input) 256)))
