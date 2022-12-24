(ns year-2022.day-20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [utils.bimap :refer [bimap rmap]]))

#_(def input [1 2 -3 3 -2 0 4])
(def input (mapv parse-long (str/split-lines (slurp (io/resource "2022/20/input")))))

(defn mixn [input bmap source-idx]
  ;; Get the value at the source index
  (let [v (nth input source-idx)
        current-out-idx (bmap source-idx)
        next-out-idx (mod (+ current-out-idx v) (dec (count input)))
        forwards? (> next-out-idx current-out-idx)]
    (assoc
     (if forwards?
       (let [idx-range (range (inc current-out-idx) (inc next-out-idx))
             to-move (map (rmap bmap) idx-range)]
         (reduce #(update %1 %2 dec) bmap to-move))
       (let [idx-range (range next-out-idx current-out-idx)
             to-move (map (rmap bmap) idx-range)]
         (reduce #(update %1 %2 inc) bmap (reverse to-move))))
     source-idx next-out-idx)))

(defn mix [input bmap]
  (reduce (partial mixn input) bmap (range (count input))))

(def part-1
  (let [start-bmap (->> (range (count input))
                        (map-indexed vector)
                        flatten
                        (apply bimap))
        mixed (mix input start-bmap)
        mixed-out (map input (vals (rmap mixed)))
        cycled (drop-while (partial not= 0) (cycle mixed-out))]
    (+ (nth cycled 1000)
       (nth cycled 2000)
       (nth cycled 3000))))

(def decryption-key 811589153)

(defn mixnn [input bmap n]
  (nth (iterate (partial mix input) bmap) n))

(def part-2
  (let [input (mapv (partial * decryption-key) input)
        start-bmap (->> (range (count input))
                        (map-indexed vector)
                        flatten
                        (apply bimap))
        mixed (mixnn input start-bmap 10)
        mixed-out (map input (vals (rmap mixed)))
        cycled (drop-while (partial not= 0) (cycle mixed-out))]
    (+ (nth cycled 1000)
       (nth cycled 2000)
       (nth cycled 3000))))
