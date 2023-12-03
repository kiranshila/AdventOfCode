(ns year-2023.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "2023/1/input")))

(defn part-1 [input]
  (->> (str/split-lines input)
       (map (partial re-seq #"[0-9]"))
       (map #(str (first %) (last %)))
       (map parse-long)
       (reduce +)))

(defn part-2 [input]
  (->> (str/split-lines input)
       ;; Evil regex hack
       (map (partial re-seq #"(?=([0-9]|one|two|three|four|five|six|seven|eight|nine))"))
       (map (partial map second))
       (map #(vector (first %) (last %)))
       (map #(map (fn [num]
                    (if (> (count num) 1)
                      ({"one" 1 "two" 2 "three" 3
                        "four" 4 "five" 5 "six" 6
                        "seven" 7 "eight" 8 "nine" 9}
                       num)
                      (parse-long num))) %))
       (map #(str (first %) (last %)))
       (map parse-long)
       (reduce +)))
