(ns year-2022.day-25
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "2022/25/input")))

(def negatives {\= -2 \- -1})

(defn ->decimal [num]
  (reduce + (for [[idx digit] (map-indexed vector (reverse (seq num)))]
              (* (long (Math/pow 5 idx))
                 (if (contains? negatives digit)
                   (negatives digit)
                   (parse-long (str digit)))))))

(defn ->base5 [num]
  (loop [digits (list)
         remainder num]
    (let [q (quot remainder 5)
          m (mod remainder 5)]
      (if (zero? q)
        (conj digits m)
        (recur (conj digits m) q)))))

(defn base5->snafu [digits]
  (loop [digits (reverse digits)
         fixed '()
         carry 0]
    (if (or (seq digits)
            (not (zero? carry)))
      (let [this-digit (+ carry (or (first digits) 0))]
        (case this-digit
          3 (recur (rest digits) (conj fixed -2) 1)
          4 (recur (rest digits) (conj fixed -1) 1)
          5 (recur (rest digits) (conj fixed 0) 1)
          (recur (rest digits) (conj fixed this-digit) 0)))
      fixed)))

(def sum
  (->> (str/split-lines input)
       (map ->decimal)
       (reduce +)
       ->base5
       base5->snafu
       (replace {-1 \- -2 \=})
       (apply str)))
