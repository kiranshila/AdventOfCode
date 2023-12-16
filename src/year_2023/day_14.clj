(ns year-2023.day-14
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (slurp (io/resource "2023/14/input")))
(def example (slurp (io/resource "2023/14/example")))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (into {}
          (for [i (range (count lines))
                j (range (count (first lines)))
                :let [c (nth (nth lines i) j)]]
            [[(- (count lines) i) j] c]))))

(def dir-delta {\N [1 0] \S [-1 0] \E [0 1] \W [0 -1]})

(def plat-update
  (memoize
   (fn [dir plat]
     (loop [new plat
            old (seq plat)]
       (if (seq old)
         (let [[pos piece] (first old)
               slide-pos (mapv + pos (dir-delta dir))
               slide-piece (plat slide-pos)]
           (if (and (= slide-piece \.) (= piece \O))
             (recur (-> new (assoc slide-pos \O) (assoc pos \.))
                    (rest old))
             (recur new (rest old))))
         new)))))

(defn slide [platform dir]
  (loop [prev platform]
    (let [step (plat-update dir prev)]
      (if (= step prev)
        step
        (recur step)))))

(defn vis [platform]
  (let [rs (reverse (range 1 11))
        cs (range 10)]
    (print "\n")
    (doseq [r rs]
      (doseq [c cs
              :let [tile (platform [r c])]]
        (if (some? tile)
          (print tile)
          (print " ")))
      (print "\n"))))

(defn total-load [plat]
  (->> plat
       (filter #(= (second %) \O))
       (map first)
       (map first)
       (reduce +)))

(defn part-one [input]
  (-> (parse-input input)
      (slide \N)
      total-load))

(def spin-cycle [\N \W \S \E])

(defn spin [plat]
  (reduce slide plat spin-cycle))

(defn spin-to-fixed [plat]
  (loop [last-plat plat
         spin-count 1
         seen {}]
    (let [spun (spin last-plat)
          spun-hash (hash spun)]
      (if-let [last-count (seen spun-hash)]
        [last-count spin-count]
        (recur spun (inc spin-count) (conj seen [spun-hash spin-count]))))))

(defn part-two [input]
  (let [plat (parse-input input)
        [loop-start loop-end] (spin-to-fixed plat)
        idx-from-start (mod (- 1000000000 loop-start) (- loop-end loop-start))
        idx (+ loop-start idx-from-start)] ; Both state at start and end
    (->> (iterate spin plat)
         (take (inc idx))
         last
         total-load)))
