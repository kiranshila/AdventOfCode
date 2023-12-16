(ns year-2023.day-13
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

(def input (slurp (io/resource "2023/13/input")))
(def example (slurp (io/resource "2023/13/example")))

(defn mirrored? [x idx]
  (loop [l idx r (inc idx)]
    (let [xl (nth x l nil)
          xr (nth x r nil)]
      (cond
        (or (nil? xl) (nil? xr)) true
        (= xl xr) (recur (dec l) (inc r))
        :else false))))

(defn find-line-mirror [x]
  (->> (range (dec (count x)))
       (map-indexed (fn [i v] [i (mirrored? x v)]))
       (filter second)
       (map first)
       (map inc) ;; AOC is 1-indexed here
       (into #{})))

(defn find-pattern-mirror [pat-lines]
  (->> (map find-line-mirror pat-lines)
       (apply set/intersection)
       first)) ;; Presumably only one

(defn transpose [& xs]
  (apply map list xs))

(defn summarize [pat]
  (let [lines (str/split-lines pat)]
    (if-let [cm (find-pattern-mirror lines)]
      cm
      (* 100 (find-pattern-mirror (apply transpose lines))))))

(defn part-one [input]
  (->> (str/split input #"\n\n")
       (map summarize)
       (reduce +)))

(defn count-smudges [x idx]
  (loop [count 0
         l idx
         r (inc idx)]
    (let [xl (nth x l nil)
          xr (nth x r nil)]
      (cond
        (or (nil? xl) (nil? xr)) count
        (= xl xr) (recur count (dec l) (inc r))
        :else (recur (inc count) (dec l) (inc r))))))

(defn potential-smudge [x]
  (->> (range (dec (count x)))
       (map (partial count-smudges x))))

(defn find-pattern-smudge [pat-lines]
  (let [line-smudges  (map potential-smudge pat-lines)]
    (->> (apply transpose line-smudges)
         (map (partial reduce +))
         (#(.indexOf % 1))
         (#(get {-1 nil} % (inc %))))))

(defn summarize-two [pat]
  (let [lines (str/split-lines pat)]
    (if-let [cm (find-pattern-smudge lines)]
      cm
      (* 100 (find-pattern-smudge (apply transpose lines))))))

(defn part-two [input]
  (->> (str/split input #"\n\n")
       (map summarize-two)
       (reduce +)))
