(ns year-2022.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn pos-visible? [[i j] matrix]
  (let [row (matrix i)
        col ((apply mapv vector matrix) j)
        vis? #(every? (partial >  (row j)) %)]
    (or (= 0 i) (= 0 j) (= (dec (count row)) j) (= (dec (count col)) i)
        (vis? (subvec row 0 j))
        (vis? (subvec row (inc j)))
        (vis? (subvec col 0 i))
        (vis? (subvec col (inc i))))))

(defn take-until
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-until pred (rest s)))))))

(defn score [value v]
  (let [sight (count (take-until #(>= % value) v))]
    (if (= sight 0)
      (count v)
      sight)))

(defn scenic-score [[i j] matrix]
  (let [row (matrix i)
        col ((apply mapv vector matrix) j)
        s #(score (row j) %)]
    (* (s (subvec col (inc i)))
       (s (reverse (subvec col 0 i)))
       (s (subvec row (inc j)))
       (s (reverse (subvec row 0 j))))))

(->> (slurp (io/resource "2022/8/input"))
     str/split-lines
     (map seq)
     (map (partial map str))
     (mapv (partial mapv parse-long))
     (#(for [i (range (count %))
             j (range (count (% 1)))]
         ;; Part 1
         #_(pos-visible? [i j] %)
         ;; Part 2
         (scenic-score [i j] %)))
     ;; Part 1
     #_(filter true?)
     #_count
     ;; Part 2
     (apply max))
