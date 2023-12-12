(ns year-2023.day-12
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp (io/resource "2023/12/input")))
(def example (slurp (io/resource "2023/12/example")))

(defn parse-line [line]
  (let [[record groups] (str/split line #"\s+")
        groups (->> (str/split groups #",") (mapv parse-long))]
    [record groups]))

(defn matches
  ([record groups] (matches record groups 0 []))
  ([record groups curr-count trail]
   (if (not (seq record))
     (when (or (and (= 0 (count groups)) (= 0 curr-count))
               (and (= 1 (count groups)) (= curr-count (first groups))))
       (apply str trail))
     (let [head (first record)]
       (case head
         \# (recur (rest record) groups (inc curr-count) (conj trail head))
         \. (cond
              (zero? curr-count) (recur (rest record) groups 0 (conj trail head))
              (= curr-count (first groups)) (recur (rest record) (rest groups) 0 (conj trail head))
              :else nil)
         \? [(matches (conj (rest record) \#) groups curr-count trail)
             (matches (conj (rest record) \.) groups curr-count trail)])))))

(defn count-matches [[record groups]]
  (->> (matches record groups)
       flatten
       (filter some?)
       count))

(defn part-one [input]
  (->> (str/split-lines input)
       (map parse-line)
       (map count-matches)
       (reduce +)))

(defn unfold [[record groups]]
  )
