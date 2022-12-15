(ns year-2022.day-14
  (:require [clojure.java.io :as io]
            [quil.core :as q]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp (io/resource "2022/14/input")))

(defn parse [path]
  (->> (str/split path #" -> ")
       (map #(str/split % #","))
       (map (partial mapv parse-long))))

(def lines
  (->> (str/split-lines input)
       (map parse)))

(defn bi-range [start stop]
  (let [up? (> stop start)]
    (range start (if up? (inc stop) (dec stop)) (if up? 1 -1))))

(defn draw-line [[start-x start-y] [stop-x stop-y]]
  (vec
   (->> (if (= start-x stop-x)
          (interleave (repeat start-x) (bi-range start-y stop-y))
          (interleave (bi-range start-x stop-x) (repeat start-y)))
        (partition 2)
        (map vec))))

(def rocks
  (apply
   set/union
   (for [line lines
         [start stop] (partition 2 1 line)]
     (into #{} (draw-line start stop)))))

(defn sand-update [rocks sand]
  (let [filled (set/union sand rocks)
        bottom (apply max (map second rocks))]
    (loop [sand-pos [500 0]]
      (when-not (> (second sand-pos) bottom)
        (let [[d l r] (map (partial mapv + sand-pos)
                           [[0 1] [-1 1] [1 1]])]
          (cond
            (not (contains? filled d)) (recur d)
            (not (contains? filled l)) (recur l)
            (not (contains? filled r)) (recur r)
            :else (conj sand sand-pos)))))))

(def part-1
  (->> (iterate (partial sand-update rocks) #{})
       (take-while identity)
       last
       count))

(defn sand-update-2 [rocks sand]
  (let [filled (set/union sand rocks)
        bottom (+ 2 (apply max (map second rocks)))
        filled? #(or (contains? filled %) (= bottom (second %)))]
    (loop [sand-pos [500 0]]
      (let [[d l r] (map (partial mapv + sand-pos)
                         [[0 1] [-1 1] [1 1]])]
        (when (not (contains? sand [500 0]))
          (cond
            (not (filled? d)) (recur d)
            (not (filled? l)) (recur l)
            (not (filled? r)) (recur r)
            :else (conj sand sand-pos)))))))

#_(def part-2
    (->> (iterate (partial sand-update-2 rocks) #{})
         (take-while identity)
         last
         count))

;;; Viz

(defn setup []
  (q/frame-rate 60)
  (q/stroke 255 255 255)
  (q/background 0)
  (q/no-smooth)
  (q/scale 3)
  (doseq [point rocks]
    (apply q/point point)))

(def sand (atom #{}))

(defn draw []
  (q/stroke 255 0 0)
  (q/scale 3)
  (swap! sand (partial sand-update rocks))
  (doseq [point @sand]
    (apply q/point point)))

(q/defsketch regolith-reservoir
  :title "Regolith Reservoir"
  :setup setup
  :draw draw
  :size [2256 1504])
