(ns year-2023.day-6
  (:require
   [clojure.java.io :as io]))

(defn roots [a b c]
  (let [d (Math/sqrt (- (Math/pow b 2) (* 4 a c)))]
    [(/ (+ (- b) d) (* 2 a)) (/ (- (- b) d) (* 2 a))]))

(defn num-wins [race-time record]
  (let [[below above] (roots -1 race-time (- (inc record)))]
    (inc (- (Math/floor above) (Math/ceil below)))))

(* (num-wins 61 643)
   (num-wins 70 1184)
   (num-wins 90 1362)
   (num-wins 66 1041))

(num-wins 61709066 643118413621041)
