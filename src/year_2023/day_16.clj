(ns year-2023.day-16
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (slurp (io/resource "2023/16/input")))
(def example (slurp (io/resource "2023/16/example")))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (into {}
          (for [i (range (count lines))
                j (range (count (first lines)))
                :let [c (nth (nth lines i) j)]
                :when (not= c \.)]
            [[i j] c]))))

(defn bounds [m]
  (let [pos (keys m)
        max-i (reduce max (map first pos))
        max-j (reduce max (map second pos))]
    [max-i max-j]))

(def delta
  {\N [-1 0] \S [1 0] \E [0 1] \W [0 -1]})

(defn next-pos [pos dir]
  (mapv + (delta dir) pos))

(def reflection
  {[\E \\] \S [\W \\] \N [\N \\] \W [\S \\] \E
   [\E \/] \N [\W \/] \S [\N \/] \E [\S \/] \W})

(defn trace [[start-pos start-dir] m]
  (let [[max-i max-j] (bounds m)]
    (letfn
     [(inner [[i j :as pos] dir pols]
        (if (and (<= 0 i max-i)
                 (<= 0 j max-j)
                 (nil? (pols [pos dir])))
          (let [tile (m pos)
                pols (conj pols [pos dir])]
            (cond
              (and (= tile \|)
                   (#{\E \W} dir)) (->> pols
                                        (inner (next-pos pos \N) \N)
                                        (recur (next-pos pos \S) \S))
              (and (= tile \-)
                   (#{\N \S} dir)) (->> pols
                                        (inner (next-pos pos \E) \E)
                                        (recur (next-pos pos \W) \W))
              (#{\\ \/} tile) (let [next-dir (reflection [dir tile])]
                                (recur (next-pos pos next-dir) next-dir pols))
              :else (recur (next-pos pos dir) dir pols)))
          pols))]
      (->> (inner start-pos start-dir #{})
           (map first)
           (into #{})))))

(defn vis [energized]
  (let [rs (range 10)
        cs (range 10)]
    (print "\n")
    (doseq [r rs]
      (doseq [c cs
              :let [tile (energized [r c])]]
        (if (some? tile)
          (print "#")
          (print ".")))
      (print "\n"))))

(defn part-one [input]
  (->> (parse-input input)
       (trace [[0 0] \E])
       count))

(defn part-two [input]
  (let [m (parse-input input)
        [max-i max-j] (bounds m)
        top (for [j (range (inc max-j))] [[0 j] \S])
        bottom (for [j (range (inc max-j))] [[max-i j] \N])
        left (for [i (range (inc max-i))] [[i 0] \E])
        right (for [i (range (inc max-i))] [[i max-j] \W])
        edges (concat top bottom left right)]
    (->> (pmap #(count (trace % m)) edges)
         (reduce max))))
