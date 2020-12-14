(ns year-2020.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (slurp (io/resource "2020/13/input")))

(defn parse [input]
  (let [[timestamp bus-ids] (str/split-lines input)]
    [(edn/read-string timestamp) (->> (str/split bus-ids #",")
                                      (map edn/read-string)
                                      (into []))]))

(defn earliest-arrival [timestamp bus-ids]
  (apply min-key :arrival
         (for [bus-id bus-ids
               :when (not= 'x bus-id)]
           (->> (/ timestamp bus-id)
                inc
                range
                (map (partial * bus-id))
                (filter (partial <= timestamp))
                (apply min)
                (hash-map :id bus-id :arrival)))))

(defn solution-1 [input]
  (let [[timestamp bus-ids] (parse input)
        {:keys [arrival id]} (earliest-arrival timestamp bus-ids)]
    (* id (- arrival timestamp))))

; I stole this
(defn xgcd
  [a b]
  (if (= a 0)
    [b 0 1]
    (let [[g x y] (xgcd (mod b a) a)]
      [g (- y (* (Math/floorDiv b a) x)) x])))

(defn solution-2 [input]
  (let [[_ bus-ids] (parse input)
        N (->> (filter (partial not= 'x) bus-ids)
               (reduce *))]
    (mod (reduce + (for [i (range (count bus-ids))
                         :let [ni (nth bus-ids i)]
                         :when (not= 'x ni)
                         :let [div (/ N ni)
                               [_ _ si] (xgcd  ni div)]]
                     (* (- i) si div)))
         N)))

(defn solve-next-cr [state id]
  (if (= 'x id) [(first state) (second state) (inc (nth state 2))]
      [(* id (first state))
       (first (keep #(let [mul (mod (+ (second state) (* % (first state))) id)]
                       (when (= mul (mod (- id (nth state 2)) id))
                        mul))
                    (range id)))
       (inc (nth state 2))]))
