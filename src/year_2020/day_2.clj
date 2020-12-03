(ns year-2020.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "2020/2/input")))

(defn parse [input]
  (let [[i j c p] (rest (re-matches #"(\d+)-(\d+) ([a-z]): ([a-z]+)"
                                    input))]
    [(Integer/parseInt i) (Integer/parseInt j) (first c) p]))

(defn count-matches [match-fn input]
  (->> input
       str/split-lines
       (map parse)
       (map match-fn)
       (filter true?)
       count))

(defn match? [[min max char password]]
  (when-let [freq ((frequencies password) char)]
    (and (>= freq min)
         (<= freq max))))

(defn match-2? [[min max char password]]
  (or (and (= (get password (dec min)) char)
           (not= (get password (dec max)) char))
      (and (not= (get password (dec min)) char)
           (= (get password (dec max)) char))))

(defn solution-1 [input]
  (count-matches match? input))

(defn solution-2 [input]
  (count-matches match-2? input))
