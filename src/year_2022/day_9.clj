(ns year-2022.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def dirs
  {:L [-1 0] :R [1 0] :U [0 1] :D [0 -1]})

(defn update-tail [[head-pos tail-pos :as state]]
  (let [[dx dy :as delta] (mapv - head-pos tail-pos)]
    (cond
      (or (and (zero? dx)
               (zero? dy))
          (= 1 (apply max (map abs delta)))) state
      :else (let [move (mapv #(if (not (zero? %)) (/ % (abs %)) %) delta)
                  new-tail-pos (mapv + move tail-pos)]
              (assoc state 1 new-tail-pos)))))

(defn process [[state :as states] [dir n]]
  (->> state
       (iterate
        #(-> %
             (update 0 (partial mapv + (dir dirs)))
             update-tail))
       rest
       (into states (take n))))

(def movements
  (->> (slurp (io/resource "2022/9/input"))
       str/split-lines
       (map #(str/split % #" "))
       (map (fn [[x y]] [(keyword x) (parse-long y)]))))

(def part-1
  (->> movements
       (reduce process (list (vec (repeat 2 [0 0]))))
       (map second)
       (into #{})
       count))

(defn process-2 [[state :as states] [dir n]]
  (->> state
       (iterate
        #(reductions (comp second update-tail vector)
                     (mapv + (first %) (dir dirs)) (rest %)))
       rest
       (into states (take n))))

(def part-2
  (->> movements
       (reduce process-2 (list (vec (repeat 10 [0 0]))))
       (map last)
       (into #{})
       count))
