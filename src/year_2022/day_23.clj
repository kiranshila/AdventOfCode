(ns year-2022.day-23
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def input (slurp (io/resource "2022/23/input")))

(defn parse-input [input]
  (into #{}
        (for [[y line] (map-indexed vector (str/split-lines input))
              [x tile] (map-indexed vector (seq line))
              :when (not= tile \.)]
          [x y])))

(def directions
  {:N [0 -1] :NE [1 -1] :E [1 0] :SE [1 1]
   :S [0 1] :SW [-1 1] :W [-1 0] :NW [-1 -1]})

(def dir-ord [:N :S :W :E])

(def check-dirs
  {:N [:N :NE :NW]
   :S [:S :SE :SW]
   :W [:W :NW :SW]
   :E [:E :NE :SE]})

(defn can-move? [elves pos dir]
  (let [dirs (vals (select-keys directions (check-dirs dir)))
        poss (into #{} (map (partial mapv + pos) dirs))]
    (zero? (count (set/intersection elves poss)))))

(defn elf-movement [elves consider-idx elf-pos]
  (let [surrounding-positions (into #{} (map (partial mapv + elf-pos) (vals directions)))]
    (if (zero? (count (set/intersection elves surrounding-positions)))
      elf-pos
      (loop [dirs (take 4 (nthrest (cycle dir-ord) consider-idx))]
        (if (can-move? elves elf-pos (first dirs))
          (mapv + elf-pos (directions (first dirs)))
          (recur (rest dirs)))))))

(defn proposed-movement [elves round]
  (into {} (for [elf-pos elves
                 :let [proposed (elf-movement elves round elf-pos)]
                 :when (seq proposed)]
             {elf-pos proposed})))

(defn run-round [elves round]
  (let [proposed (proposed-movement elves round)
        target-freqs (frequencies (vals proposed))
        new-poss (into {} (filter #(= 1 (target-freqs (second %))) proposed))]
    (-> elves
        (#(apply disj % (keys new-poss)))
        (#(apply conj % (vals new-poss))))))

(defn rect-extrema [points]
  (let [xs (map #(nth % 0) points)
        ys (map #(nth % 1) points)
        minx (apply min xs)
        maxx (apply max xs)
        miny (apply min ys)
        maxy (apply max ys)]
    [[minx maxx] [miny maxy]]))

(def part-1
  (let [elves (parse-input input)
        final-elves (reduce run-round elves (range 10))
        [[minx maxx] [miny maxy]] (rect-extrema final-elves)
        rect-size (* (inc (- maxx minx)) (inc (- maxy miny)))]
    (- rect-size (count final-elves))))

(def part-2
  (let [elves (parse-input input)
        rounds (->> [elves 0]
                    (iterate (fn [[board round]] [(run-round board round) (inc round)]))
                    (map first))
        round-pairs (partition 2 1 rounds)]
    (->> round-pairs
         (take-while (fn [[last-board this-board]] (not= last-board this-board)))
         count
         inc)))
