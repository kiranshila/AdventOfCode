(ns year-2022.day-22
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def test-input "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5")

(def input (str/trimr (slurp (io/resource "2022/22/input"))))

(def path-parser (insta/parser "<r>=(m|t)+\nm=#'\\d+'\nt='R'|'L'"))

(defn parse-path [path-line]
  (insta/transform {:m #(vector :m (parse-long %))} (path-parser path-line)))

(defn parse-input [input]
  (let [[map path] (str/split input #"\n\n")
        map (apply merge
                   (for [[y line] (map-indexed vector (str/split-lines map))
                         [x tile] (map-indexed vector (seq line))
                         :when (not= tile \space)]
                     {[x y] ({\. :open \# :wall} tile)}))]
    [map (parse-path path)]))

(def next-dir
  {[:n "R"] :e [:n "L"] :w
   [:e "R"] :s [:e "L"] :n
   [:s "R"] :w [:s "L"] :e
   [:w "R"] :n [:w "L"] :s})

(def deltas
  {:n [0 -1] :s [0 1] :e [1 0] :w [-1 0]})

(defn next-pos [map [cx cy :as current] dir]
  (let [pos (mapv + (dir deltas) current)]
    (if (contains? map pos)
      pos
      (letfn [(scol [] (sort-by second (filter #(= (first %) cx) (keys map))))
              (srow [] (sort-by first (filter #(= (second %) cy) (keys map))))]
        (case dir
          :n (last (scol))
          :s (first (scol))
          :e (first (srow))
          :w (last (srow)))))))

(defn move [map pos dir n]
  (loop [n n
         pos pos]
    (let [new-pos (next-pos map pos dir)]
      (if (or (zero? n) (= :wall (map new-pos)))
        pos
        (recur (dec n)
               new-pos)))))

(defn perform-instruction [map [pos facing] [action arg]]
  (case action
    :t [pos (next-dir [facing arg])]
    :m [(move map pos facing arg) facing]))

(defn walk-path [map instructions]
  (let [top-left (->> map
                      keys
                      (filter #(= 0 (second %)))
                      (sort-by first)
                      first)]
    (reduce (partial perform-instruction map) [top-left :e] instructions)))

(def part-1
  (let [[[c r] facing] (->> (parse-input input)
                            (apply walk-path))
        facing ({:e 0 :s 1 :w 2 :n 3} facing)]
    (+ (* 1000 (inc r))
       (* 4 (inc c))
       facing)))

(defn face [map n-sides pos])
