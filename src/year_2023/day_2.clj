(ns year-2023.day-2
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def input (slurp (io/resource "2023/1/input")))

(defn parse-cubes [cubes]
  (let [[[_ n c]] (re-seq #"\s+(\d+) (red|green|blue)" cubes)]
    [(keyword c) (parse-long n)]))

(defn parse-game [game]
  (let [[_ rest] (str/split game #":")
        subsets (->> (str/split rest #";")
                     (map #(str/split % #","))
                     (map #(map parse-cubes %))
                     (map #(into {:red 0 :green 0 :blue 0} %)))]
    subsets))

(defn possible-round? [r g b {:keys [red green blue]}]
  (and (<= red r) (<= green g) (<= blue b)))

(defn possible-game? [r g b game]
  (every? (partial possible-round? r g b) game))

(defn part-1 [input]
  (->> (str/split-lines input)
       (map-indexed #(vector (inc %1) (parse-game %2)))
       (filter (fn [[_ g]] (possible-game? 12 13 14 g)))
       (map first)
       (reduce +)))

(defn possible-game [game]
  {:red (apply max (map :red game))
   :green (apply max (map :green game))
   :blue (apply max (map :blue game))})

(defn part-2 [input]
  (->> (str/split-lines input)
       (map parse-game)
       (map possible-game)
       (map vals)
       (map (partial reduce *))
       (reduce +)))
