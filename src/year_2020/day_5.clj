(ns year-2020.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "2020/5/input")))

(def bin-pair {"F" "0" "B" "1" "L" "0" "R" "1"})

(defn pass-num [pass]
  (letfn [(binify [s] (Integer/parseInt (str/replace s #"F|B|L|R" bin-pair) 2))]
    (let [row (binify (subs pass 0 7))
          col (binify (subs pass 7 10))]
      (+ col (* row 8)))))

(defn solution-1 [input]
  (let [passes (str/split-lines input)]
    (apply max (map pass-num passes))))

(def rows 128)
(def cols 8)

(defn solution-2 [input]
  (->> (let [passes (str/split-lines input)
             ids (into #{} (map pass-num passes))]
         (for [row (range 3 (- rows 4))
               col (range cols)]
           (let [this-id (+ col (* row 8))]
             (when (not (contains? ids this-id))
               this-id))))
       (filter identity)
       first))
