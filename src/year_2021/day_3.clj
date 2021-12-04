(ns year-2021.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "2021/3/input"))
                str/split-lines))

(defn reducer [sum number-str]
  (->> (map #(Character/digit % 2) number-str)
       (map + sum)))

(defn part1 [strs]
  (let [N (double (count strs))
        n (count (first strs))]
    (->> (reduce reducer (take n (repeat 0)) strs)
         (map #(Math/round (/ % N)))
         (apply str)
         (#(Long/parseLong % 2))
         (#(* % (bit-and (- (int (Math/pow 2 n)) 1)
                         (bit-not %)))))))

(part1 input)

(defn bit-criteria
  ([numbers cmp] (bit-criteria numbers 0 cmp))
  ([numbers bit cmp]
   (if (not= (count numbers) 1)
     (let [digit-count (->> (map #(nth % bit) numbers)
                            frequencies)
           digit (if (cmp (digit-count \0 0) (digit-count \1 0)) \0 \1)]
       (recur (filter #(= (nth % bit) digit) numbers) (+ bit 1) cmp))
     (Long/parseLong (first numbers) 2))))

(defn part2 [strs]
  (* (bit-criteria strs >) (bit-criteria strs <=)))

(part2 input)
