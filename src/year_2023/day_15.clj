(ns year-2023.day-15
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [flatland.ordered.map :refer [ordered-map]]))

(def input (str/trim (slurp (io/resource "2023/15/input"))))
(def example (str/trim (slurp (io/resource "2023/15/example"))))

(defn elf-hash
  ([input] (elf-hash input 0))
  ([input current-val]
   (if (seq input)
     (let [code (int (first input))]
       (recur (rest input) (rem (* 17 (+ current-val code)) 256)))
     current-val)))

(defn part-one [input]
  (->> (str/split input #",")
       (map elf-hash)
       (reduce +)))

(defn elf-hashmap [m op]
  (if (str/ends-with? op "-")
    (let [[label] (str/split op #"-")
          hash (elf-hash label)]
      (update-in m [hash] dissoc label))
    (let [[label focal-length] (str/split op #"=")
          hash (elf-hash label)
          contents (get m hash (ordered-map))]
      (assoc m hash (assoc contents label (parse-long focal-length))))))

(defn focusing-power [m]
  (->> (for [[box contents] m]
         (map-indexed (fn [idx [_ v]] (* (inc box) (inc idx) v)) contents))
       flatten
       (reduce +)))

(defn part-two [input]
  (->> (str/split input #",")
       (reduce elf-hashmap {})
       focusing-power))
