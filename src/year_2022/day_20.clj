(ns year-2022.day-20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input [1 2 -3 3 -2 0 4])
#_(def input (map parse-long (str/split-lines (slurp (io/resource "2022/20/input")))))

(defn remove-at [v n]
  (into (subvec v 0 n) (subvec v (inc n))))

(defn insert-at [v elem n]
  (let [[l r] (split-at n v)]
    (apply merge (vec l) elem (vec r))))

(defn numberwang [input n]
  (let [val (nth input n)
        idx (mod (+ val n) (dec n))]
    (remove-at (insert-at input val idx) n)))
