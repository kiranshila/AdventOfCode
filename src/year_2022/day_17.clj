(ns year-2022.day-17
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def test-input (seq ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"))
(def input (seq (str/trim (slurp (io/resource "2022/17/input")))))

(def tetromino
  {:h #{[0 0] [1 0] [2 0] [3 0]}
   :p #{[1 2] [0 1] [1 1] [2 1] [1 0]}
   :l #{[2 2] [2 1] [0 0] [1 0] [2 0]}
   :v #{[0 3] [0 2] [0 1] [0 0]}
   :s #{[0 0] [0 1] [1 0] [1 1]}})

(def tet-order {:h :p :p :l :l :v :v :s :s :h})

(defn collision? [board test-set]
  (or (not (every? #(<= 0 (first %) 6) test-set))
      (not (every? #(>= (second %) 0) test-set))
      (not= 0 (count (set/intersection board test-set)))))

(defn spawn-rock [tet-set last-highest]
  (->> tet-set
       (mapv #(update % 1 + last-highest 3))
       (mapv #(update % 0 + 2))
       (into #{})))

(defn push [tet-set dir]
  (case dir
    \< (into #{} (mapv #(update % 0 - 1) tet-set))
    \> (into #{} (mapv #(update % 0 + 1) tet-set))))

(defn fall [tet-set]
  (->> tet-set
       (mapv #(update % 1 + -1))
       (into #{})))

(defn play-new-tet [board jet-seq jet-seq-idx current-tet highest]
  (loop [rock (spawn-rock (tetromino current-tet) highest)
         jet-seq-idx jet-seq-idx]
    (let [pushed-rock (push rock (nth jet-seq jet-seq-idx))
          rock (if (collision? board pushed-rock) rock pushed-rock)
          fallen-rock (fall rock)
          next-jet-idx (mod (inc jet-seq-idx) (count jet-seq))]
      (if (collision? board fallen-rock)
        [rock next-jet-idx]
        (recur fallen-rock next-jet-idx)))))

(defn falling [n-rocks jet-sequence]
  (loop [board #{}
         current-tet :h
         highest 0
         rock-count 0
         jet-seq-idx 0]
    (if (= rock-count n-rocks)
      highest
      (let [[new-rock next-idx] (play-new-tet board jet-sequence jet-seq-idx current-tet highest)
            new-board (into board new-rock)]
        (recur new-board
               (tet-order current-tet)
               (inc (apply max (map second new-board)))
               (inc rock-count)
               next-idx)))))

(defn meaningful-board-state [board n]
  (let [top-y (apply max (conj (map second board) 0))
        pieces-we-care-about (filter #(>= top-y (second %) (- top-y n)) board)
        min-of-these (apply min (conj (map second pieces-we-care-about) top-y))]
    (into #{} (mapv #(update % 1 - min-of-these) pieces-we-care-about))))

(defn falling-cycle-detect [jet-sequence]
  (loop [board #{}
         current-tet :h
         highest 0
         rock-count 0
         jet-seq-idx 0
         tracker {}]
    (let [tracked {:shape current-tet :idx jet-seq-idx :state (meaningful-board-state board 25)}]
      (if (contains? tracker tracked)
        [(tracker tracked) (- rock-count (tracker tracked))]
        (let [[new-rock next-idx] (play-new-tet board jet-sequence jet-seq-idx current-tet highest)
              new-board (into board new-rock)
              max-y (apply max (map second new-board))]
          (recur new-board
                 (tet-order current-tet)
                 (inc max-y)
                 (inc rock-count)
                 next-idx
                 (assoc tracker tracked rock-count)))))))

(defn part-2 [jet-seq]
  (let [[preamble-len cycle-len] (falling-cycle-detect jet-seq)
        cycleable (- 1000000000000 preamble-len)
        complete-cycles (quot cycleable cycle-len)
        leftover (mod cycleable cycle-len)
        preamble-height (falling preamble-len jet-seq)
        total-cycle-height (- (falling (+ preamble-len cycle-len) jet-seq) preamble-height)
        partial-cycle-height (- (falling (+ preamble-len leftover) jet-seq) preamble-height)]
    (+ preamble-height partial-cycle-height (* complete-cycles total-cycle-height))))
