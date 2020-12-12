(ns year-2020.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "2020/11/input")))

(def position {\L :empty
               \# :occup
               \. :floor})

(def adjacents [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn read-board [input]
  (let [lines (str/split-lines input)]
    (into [] (for [line lines]
               (into [] (map position line))))))

(defn get-adjacents [board position]
  (let [cols (count (first board))
        rows (count board)]
    (for [offset adjacents
          :let [[row col] (map + position offset)]
          :when (and (<= 0 col (dec cols))
                     (<= 0 row (dec rows)))]
      (get-in board [row col]))))

(defn raycast [board position direction]
  (lazy-seq (cons (get-in board position)
                  (raycast board (map + position direction) direction))))

(defn first-raycast-seat [board position direction]
  (->> (raycast board position direction)
       (filter (complement #{:floor}))
       rest
       first))

(defn get-adjacents-raycast [board position]
  (for [direction adjacents]
    (first-raycast-seat board position direction)))

(defn evolve [board adj-fn adj-count]
  (let [cols (count (first board))
        rows (count board)]
    (into [] (for [row (range rows)]
       (into [] (for [col (range cols)
                      :let [adjs (adj-fn board [row col])
                            seat (get-in board [row col])]]
                  (cond
                    (and (= seat :empty)
                         (empty? (filter #{:occup} adjs))) :occup
                    (and (= seat :occup)
                         (>= (count (filter #{:occup} adjs)) adj-count)) :empty
                    :else seat)))))))

(defn solution-1 [input]
  (let [board (read-board input)]
    (loop [last-board board
           this-board (evolve board get-adjacents 4)]
      (if (= this-board last-board)
        (reduce + (map #(count (filter #{:occup} %)) this-board))
        (recur this-board (evolve this-board get-adjacents 4))))))

(defn solution-2 [input]
  (let [board (read-board input)]
    (loop [last-board board
           this-board (evolve board get-adjacents-raycast 5)]
      (if (= this-board last-board)
        (reduce + (map #(count (filter #{:occup} %)) this-board))
        (recur this-board (evolve this-board get-adjacents-raycast 5))))))
