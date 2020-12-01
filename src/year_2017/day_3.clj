(ns year-2017.day-3
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "2017/3/input"))
                (str/trim)
                (edn/read-string)))

(defn solution-1 [input]
  (if (== input 1)
    0
    (let [ring-number (let [nearest-square (Math/ceil (Math/sqrt input))]
                       (if (odd? (int nearest-square))
                         nearest-square
                         (inc nearest-square)))
         ring (dec (/ (inc ring-number) 2))
         displacement (mod (+ input (dec ring)) (dec ring-number))]
     (+ ring displacement))))

; Looked up OEIS for part 2
