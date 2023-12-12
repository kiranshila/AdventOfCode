(ns year-2023.day-11
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as comb]
   [clojure.set :as set]))

(def example (slurp (io/resource "2023/11/example")))
(def input (slurp (io/resource "2023/11/input")))

(defn parse-image [input]
  (->> (str/split-lines input)
       (map seq)
       (map #(map-indexed vector %))
       (map #(filter (fn [[_ c]] (= c \#)) %))
       (map #(map first %))
       (map #(into #{} %))
       (map-indexed vector)
       (into {})))

(defn missing [image]
  {:row (->> (filter (fn [[_ v]] (empty? v)) image)
             (map first)
             (into #{}))
   :col (let [not-missing (apply set/union (vals image))
              max-col (reduce max (seq not-missing))
              all-cols (into #{} (range (inc max-col)))]
          (set/difference all-cols not-missing))})

(defn pairs [image]
  (comb/combinations (for [[r cols] image c cols] [r c]) 2))

(defn extra [ms a b]
  (->> (range a (inc b))
       (into #{})
       (set/intersection ms)
       count))

(defn compute-dist [ms scale [r1 c1] [r2 c2]]
  (+ (abs (- r1 r2)) (abs (- c1 c2))
     (* (dec scale) (extra (:row ms) (min r1 r2) (max r1 r2)))
     (* (dec scale) (extra (:col ms) (min c1 c2) (max c1 c2)))))

(defn sum-dists [input scale]
  (let [image (parse-image input)
        ms (missing image)
        ps (pairs image)]
    (reduce +
            (for [[g1 g2] ps]
              (compute-dist ms scale g1 g2)))))

; Part 1
(sum-dists input 2)
; Part 2
(sum-dists input 1000000)
