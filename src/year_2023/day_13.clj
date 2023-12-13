(ns year-2023.day-13
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

(def input (slurp (io/resource "2023/13/input")))
(def example (slurp (io/resource "2023/13/example")))

(defn overlaps? [[a b]]
  (if (and (seq a) (seq b))
    (if (= (first a) (first b))
      (recur [(rest a) (rest b)])
      false)
    true))

(defn mirrors [row]
  (->> (for [i (range 1 (count row))
             :let [left (reverse (take i row))
                   right (drop i row)]]
         [left right])
       (map overlaps?)
       (map-indexed vector)
       (filter second)
       (map first)
       (map inc) ;; Unsure why I'm off by one
       (into #{})))

(defn transpose [& xs]
  (apply map list xs))

(defn summarize [pat]
  (let [lines (str/split-lines pat)
        col-mirrors (->> lines
                         (map mirrors)
                         (apply set/intersection))
        row-mirrors (->> lines
                         (apply transpose)
                         (map mirrors)
                         (apply set/intersection))]
    (+ (reduce + col-mirrors)
       (reduce + (map (partial * 100) row-mirrors)))))

(defn part-one [input]
  (->> (str/split input #"\n\n")
       (map summarize)
       (reduce +)))

;; Find all the points that break the reflection
;; Swap them all and

(defn find-smudge
  ([[a b i]]
   (when (and (seq a) (seq b))
     (if (= (first a) (first b))
       (recur [(rest a) (rest b) (dec i)])
       i))))

(defn smudge [row]
  (let [prev-refl (mirrors row)]
    (->> (for [i (set/difference (into #{} (range 1 (count row))) prev-refl)
               :let [left (reverse (take i row))
                     right (drop i row)]]
           [left right i])
         (map find-smudge)
         (into #{}))))

(defn pat-smudge [pat]
  (let [lines (str/split-lines pat)
        col-smudge (->> lines
                        (map smudge)
                        (apply set/intersection))
        row-smudge (->> lines
                        (apply transpose)
                        (map smudge)
                        (apply set/intersection))]
    [col-smudge row-smudge]))
