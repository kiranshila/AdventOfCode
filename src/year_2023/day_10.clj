(ns year-2023.day-10
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.math :as math]))

(def input (slurp (io/resource "2023/10/input")))
(def example (slurp (io/resource "2023/10/example")))

(def delta
  {\N [-1 0] \S [1 0] \E [0 1] \W [0 -1]})

(def valid
  {\N #{\F \7 \|} \S #{\L \J \|} \E #{\J \7 \-} \W #{\F \L \-}})

(def connection
  {\| #{\N \S} \- #{\E \W} \L #{\N \E} \J #{\N \W} \7 #{\S \W} \F #{\S \E}})

(defn parse-pipes [input]
  (->> (str/split-lines input)
       (map-indexed vector)
       (map (fn [[i row]]
              (for [[j c] (map-indexed vector row)
                    :when (not= c \.)]
                [[i j] c])))
       (filter seq)
       (map #(into {} %))
       (apply merge)))

(defn v+ [v vs]
  (map (partial mapv + v) vs))

(defn next-valid [pipe-map visited pos]
  (->> (pipe-map pos)
       connection
       (map delta)
       (v+ pos)
       (into #{})
       (#(set/difference % visited))
       first))

(defn next-start [pipe-map start]
  (first
   (for [[delta valid] (map (juxt delta valid) [\N \S \E \W])
         :let [pos (mapv + start delta)
               c (pipe-map pos)]
         :when (valid c)]
     pos)))

(defn trace-path [pipe-map]
  (let [start (first (keep (fn [[k v]] (when (= \S v) k)) pipe-map))]
    (loop [visited #{start}
           path [[start (pipe-map start)]]
           current (next-start pipe-map start)]
      (if-let [next (next-valid pipe-map visited current)]
        (recur (conj visited current) (conj path [current (pipe-map current)]) next)
        (conj path [current (pipe-map current)])))))

(defn part-one [input]
  (->> (parse-pipes input)
       trace-path
       count
       (#(quot % 2))))

(def foo (->> (parse-pipes example) trace-path))

; Using the second derivative
(defn normals [path]
  (let [path (map first path)
        wrap (conj (into [] (conj path (last path))) (first path))]
    (into {}
          (for [i (range 1 (inc (count path)))
                :let [pos (nth wrap i)
                      [pr pc] (nth wrap (dec i))
                      [nr nc] (nth wrap (inc i))
                      dr (- nr pr)
                      dc (- nc pc)]]
            [pos (case [dr dc]
                   [2 0] #{\W}
                   [1 1] #{\W \S}
                   [0 2] #{\S}
                   [-1 1] #{\S \E}
                   [-2 0] #{\E}
                   [-1 -1] #{\E \N}
                   [0 -2] #{\N}
                   [1 -1] #{\N \W})]))))

(def boxc {\L \╚ \J \╝ \- \═ \| \║ \7 \╗ \F \╔ \S \S})
(def norc
  {#{\N} \↑
   #{\E} \→
   #{\W} \←
   #{\S} \↓
   #{\N \W} \↖
   #{\N \E} \↗
   #{\S \W} \↙
   #{\S \E} \↘})

(defn vis [path]
  (let [pretty (into {} (map #(update % 1 boxc) path))
        max-i (reduce max (map first (keys pretty)))
        max-j (reduce max (map second (keys pretty)))]
    (doseq [i (range (inc max-i))]
      (doseq [j (range (inc max-j))]
        (print (get pretty [i j] "+")))
      (print "\n"))))

(defn vis-norm [path]
  (let [max-i (reduce max (map first (map first path)))
        max-j (reduce max (map second (map first path)))
        norms (normals path)]
    (doseq [i (range (inc max-i))]
      (doseq [j (range (inc max-j))]
        (if-let [norm (norms [i j])]
          (print (norc norm))
          (print " ")))
      (print "\n"))))

(defn raytrace-area [path]
  (let [max-i (reduce max (map first (map first path)))
        max-j (reduce max (map second (map first path)))
        norms (normals path)]
    (reduce +
     (for [i (range max-i)]
       (loop [norm-count 0
              area 0
              j 0]
         (if (= j (inc max-j))
           area
           (let [norm (norms [i j])]
             (case norm
               #{\N \E} (recur (dec norm-count) area (inc j))
               #{\S \E} (recur (dec norm-count) area (inc j))
               #{\N \W} (recur (inc norm-count) area (inc j))
               #{\S \W} (recur (inc norm-count) area (inc j))
               #{\E} (recur (- norm-count 2) area (inc j))
               #{\W} (recur (+ norm-count 2) area (inc j))
               #{\S} (recur norm-count area (inc j))
               #{\N} (recur norm-count area (inc j))
               nil (if (zero? norm-count)
                     (recur norm-count area (inc j))
                     (recur norm-count (inc area) (inc j)))))))))))

(defn part-two [input]
  (->> (parse-pipes input)
       trace-path
       raytrace-area))
