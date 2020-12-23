(ns year-2020.day-22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]))

(def input (slurp (io/resource "2020/22/input")))

(def queue clojure.lang.PersistentQueue/EMPTY)

(defn parse-cards [input]
  (->> (re-seq #"Player \d:\n([\d?\n]+)" input)
       (map second)
       (map str/split-lines)
       (map #(map edn/read-string %))
       (map #(into queue %))))

(defn score [deck]
  (reduce + (map * deck (range (count deck) 0 -1))))

(defn play-combat [[p1 p2]]
  (loop [p1 p1
         p2 p2]
    (cond
      (empty? p1) p2
      (empty? p2) p1
      :else
      (let [p1-top (peek p1)
            p2-top (peek p2)]
        (if (> p1-top p2-top)
          (recur (conj (pop p1) p1-top p2-top) (pop p2))
          (recur (pop p1) (conj (pop p2) p2-top p1-top)))))))

(defn solution-1 [input]
  (->> input
       parse-cards
       play-combat
       score))

(defn take-queue [n q]
  (into queue (take n q)))

(defn play-recursive-combat [[p1 p2]]
  (loop [p1 p1
         p2 p2
         history #{}]
    (cond
      (empty? p1) [:p2 p2]
      (empty? p2) [:p1 p1]
      (contains? history [p1 p2]) [:p1 p1]
      :else
      (let [p1-top (peek p1)
            p2-top (peek p2)]
        (if (if (and (> (count p1) p1-top)
                     (> (count p2) p2-top))
              (= :p1 (first (play-recursive-combat [(take-queue p1-top (pop p1))
                                                    (take-queue p2-top (pop p2))])))
              (> p1-top p2-top))
          (recur (conj (pop p1) p1-top p2-top)
                 (pop p2)
                 (conj history [p1 p2]))
          (recur (pop p1)
                 (conj (pop p2) p2-top p1-top)
                 (conj history [p1 p2])))))))

(defn solution-2 [input]
  (->> input
       parse-cards
       play-recursive-combat
       second
       score))
