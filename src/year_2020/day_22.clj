(ns year-2020.day-21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp (io/resource "2020/21/input")))

(defn parse-ingredients [line]
  (let [[ingreds alergens] (str/split line #"\(contains ")
        ingreds (into #{} (map keyword (str/split (str/trim ingreds) #" ")))
        alergens (into #{} (map keyword (map str/trim (str/split (apply str (butlast alergens)) #","))))]
    [ingreds alergens]))

(defn solved? [alergen-map]
  (apply (partial = 1) (map count (vals alergen-map))))

(defn collect-solved [alergen-map]
  (apply set/union (map second (filter (fn [[k v]] (= 1 (count v))) alergen-map))))

(defn solve [alergen-map]
  (loop [alergen-map alergen-map]
    (if (solved? alergen-map)
      alergen-map
      (let [solved (collect-solved alergen-map)]
        (recur (into {} (map (fn [[k v]] [k (if (= 1 (count v)) v (set/difference v solved))]) alergen-map)))))))

(defn solve-alergens [ingredients]
  (let [alergens (apply  set/union (map second ingredients))
        alergen-map (apply merge (for [alergen alergens]
                                   {alergen (->> ingredients
                                                 (filter #(contains? (second %) alergen))
                                                 (map first)
                                                 (apply set/intersection))}))]
    (solve alergen-map)))

(defn solution-1 [input]
  (let [lines (str/split-lines input)
        ingredients (map parse-ingredients lines)
        ingredient-alergen-map (solve-alergens ingredients)
        alergic-ingredients (apply set/union (vals ingredient-alergen-map))]
    (->> ingredients
         (map first)
         (map #(set/difference % alergic-ingredients))
         (map count)
         (reduce +))))

(defn solution-2 [input]
  (let [lines (str/split-lines input)
        ingredients (map parse-ingredients lines)
        ingredient-alergen-map (solve-alergens ingredients)]
    (->> ingredient-alergen-map
         (into (sorted-map))
         vals
         (map first)
         (map name)
         (str/join ","))))
