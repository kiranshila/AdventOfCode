(ns year-2022.day-24
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

#_(def input
    "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#")

(def input (slurp (io/resource "2022/24/input")))

(defn middle [s]
  (butlast (rest s)))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    {:blizz
     (vec
      (for [[y line] (->> (middle lines)
                          (map-indexed vector))
            [x tile] (->> (middle (seq line))
                          (map-indexed vector))
            :when (not= tile \.)]
        [tile [x y]]))
     :size [(dec (dec (count (first lines))))
            (dec (dec (count lines)))]}))

(def dir-delt {\> [1 0] \< [-1 0] \v [0 1] \^ [0 -1] \\ [0 0]})

(defn blizz-move [size n [dir pos]]
  [dir (mapv mod (map + (map (partial * n) (dir-delt dir)) pos) size)])

(def board-state
  (memoize
   (fn [{:keys [blizz size] :as board} n]
     (assoc board :blizz (mapv (partial blizz-move size n) blizz)))))

(defn board-stop [board]
  (mapv + [0 1] (mapv dec (board :size))))

(def next-pos
  (memoize
   (fn [board pos]
     (let [ns (-> (fn  [[x y]] (or
                                (= [x y] [0 -1])
                                (= [x y] (board-stop board))
                                (and (<= 0 x (dec (first (board :size))))
                                     (<= 0 y (dec (second (board :size)))))))
                  (filter (map (partial mapv + pos) (vals dir-delt))))
           blizzards (into #{} (map second (board :blizz)))]
       (->> (filter (comp not (partial contains? blizzards)) ns))))))

(defn solve [board start stop start-time]
  (let [start {:pos start :time start-time}]
    (loop [to-visit (conj (clojure.lang.PersistentQueue/EMPTY) start)
           explored #{start}]
      (when (seq to-visit)
        (let [current (first to-visit)]
          (if (= (current :pos) stop)
            (current :time)
            (let [ns (next-pos (board-state board (inc (current :time))) (current :pos))
                  next (map #(hash-map :pos % :time (inc (current :time))) ns)
                  not-explored (set/difference (into #{} next) explored)]
              (recur (into (pop to-visit) not-explored)
                     (into explored not-explored)))))))))

(def part-1
  (let [board (parse-input input)]
    (solve board [0 -1] (board-stop board) 0)))

(def part-2
  (let [board (parse-input input)
        there part-1
        back (solve board (board-stop board) [0 -1] there)
        and-again (solve board [0 -1] (board-stop board) back)]
    and-again))
