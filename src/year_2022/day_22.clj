(ns year-2022.day-22
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.java.io :as io]))

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

(defn range2 [[startx stopx] [starty stopy] step]
  (if (= startx stopx)
    (map #(vector startx %) (range starty stopy step))
    (map #(vector % starty) (range startx stopx step))))

(def reciprocal {:n :s :s :n :e :w :w :e})

(defn stitch [edge-a edge-b dir-a dir-b]
  (merge (zipmap (map (partial vector dir-a) edge-a)
                 (map (partial vector dir-b) edge-b))
         (zipmap (map (partial vector (reciprocal dir-b)) edge-b)
                 (map (partial vector (reciprocal dir-a)) edge-a))))

;; Fuck you eric making the test a completley different shape from the actual input

(defn stitch16 [n]
  (let [edge-1 (range2 [0 n] [(dec (* 4 n)) (dec (* 4 n))] 1)
        edge-6 (range2 [(* 2 n) (* 3 n)] [0 0] 1)]
    (stitch edge-1 edge-6 :s :s)))

(defn stitch13 [n]
  (let [edge-1 (range2 [(dec n) (dec n)] [(* 3 n) (* 4 n)] 1)
        edge-3 (range2 [n (* 2 n)] [(dec (* 3 n)) (dec (* 3 n))] 1)]
    (stitch edge-1 edge-3 :e :n)))

(defn stitch36 [n]
  (let [edge-3 (range2 [(dec (* 2 n)) (dec (* 2 n))] [(* 2 n) (* 3 n)] 1)
        edge-6 (range2 [(dec (* 3 n)) (dec (* 3 n))] [(dec n) (dec 0)] -1)]
    (stitch edge-3 edge-6 :e :w)))

(defn stitch46 [n]
  (let [edge-4 (range2 [(dec (* 2 n)) (dec (* 2 n))] [n (* 2 n)] 1)
        edge-6 (range2 [(* 2 n) (* 3 n)] [(dec n) (dec n)] 1)]
    (stitch edge-4 edge-6 :e :n)))

(defn stitch42 [n]
  (let [edge-4 (range2 [n n] [n (* 2 n)] 1)
        edge-2 (range2 [0 n] [(* 2 n) (* 2 n)] 1)]
    (stitch edge-4 edge-2 :w :s)))

(defn stitch52 [n]
  (let [edge-5 (range2 [n n] [0 n] 1)
        edge-2 (range2 [0 0] [(dec (* 3 n)) (dec (* 2 n))] -1)]
    (stitch edge-5 edge-2 :w :e)))

(defn stitch51 [n]
  (let [edge-5 (range2 [n (* 2 n)] [0 0] 1)
        edge-1 (range2 [0 0] [(* 3 n) (* 4 n)] 1)]
    (stitch edge-5 edge-1 :n :e)))

(defn stitch-cube [n]
  (merge (stitch16 n) (stitch13 n) (stitch36 n) (stitch46 n) (stitch42 n) (stitch52 n) (stitch51 n)))

(def edges (stitch-cube 50))

;; Part-1 position update
#_(defn next-pos [map [cx cy :as current] dir]
    (let [pos (mapv + (dir deltas) current)]
      (if (contains? map pos)
        [dir new-pos]
        (letfn [(scol [] (sort-by second (filter #(= (first %) cx) (keys map))))
                (srow [] (sort-by first (filter #(= (second %) cy) (keys map))))]
          [dir
           (case dir
             :n (last (scol))
             :s (first (scol))
             :e (first (srow))
             :w (last (srow)))]))))

(defn next-pos [map current dir]
  (let [pos (mapv + (dir deltas) current)]
    (if (contains? map pos)
      [dir pos]
      (edges [dir current]))))

(defn move [map pos dir n]
  (loop [n n
         pos pos
         dir dir]
    (let [[new-dir new-pos] (next-pos map pos dir)]
      (if (or (zero? n) (= :wall (map new-pos)))
        [pos dir]
        (recur (dec n)
               new-pos
               new-dir)))))

(defn perform-instruction [map [pos facing] [action arg]]
  (case action
    :t [pos (next-dir [facing arg])]
    :m (move map pos facing arg)))

(defn walk-path [map instructions]
  (let [top-left (->> map
                      keys
                      (filter #(= 0 (second %)))
                      (sort-by first)
                      first)]
    (reduce (partial perform-instruction map) [top-left :e] instructions)))

(def solve
  (let [[[c r] facing] (->> (parse-input input)
                            (apply walk-path))
        facing ({:e 0 :s 1 :w 2 :n 3} facing)]
    (+ (* 1000 (inc r))
       (* 4 (inc c))
       facing)))
