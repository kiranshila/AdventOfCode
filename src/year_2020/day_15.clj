(ns year-2020.day-15
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (map edn/read-string (str/split "0,13,1,8,6,15" #",")))

(defn solution [input target]
  (loop [nums-said (into {} (map vector (butlast input) (range 1 (count input))))
         turn (inc (count input))
         last-spoken (last input)]
    (if (= turn (inc target))
      last-spoken
      (recur (assoc nums-said last-spoken (dec turn))
             (inc turn)
             (if (contains? nums-said last-spoken)
               (- (dec turn) (get nums-said last-spoken))
               0)))))
