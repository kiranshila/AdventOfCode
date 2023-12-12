(ns year-2023.day-12
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (slurp (io/resource "2023/12/input")))
(def example (slurp (io/resource "2023/12/example")))

(defn parse-line [line]
  (let [[record groups] (str/split line #"\s+")
        groups (->> (str/split groups #",") (mapv parse-long))]
    [record groups]))

(def matches
  (memoize
   (fn [record groups curr-count]
     (if (not (seq record))
       (if (or (and (= 0 (count groups)) (= 0 curr-count))
               (and (= 1 (count groups)) (= curr-count (first groups))))
         1 0)
       (let [head (first record)]
         (case head
           \# (matches (rest record) groups (inc curr-count))
           \. (cond
                (zero? curr-count) (matches (rest record) groups 0)
                (= curr-count (first groups)) (matches (rest record) (rest groups) 0)
                :else 0)
           \? (+ (matches (conj (rest record) \#) groups curr-count)
                 (matches (conj (rest record) \.) groups curr-count))))))))

(defn unfold [[record groups]]
  (let [rr (interpose \? (take 5 (cycle (list record))))
        rg (take 5 (cycle (list groups)))]
    [(apply str rr) (into [] (flatten rg))]))

(defn solve [input]
  (->> (str/split-lines input)
       (map parse-line)
       #_(map unfold)
       (map #(matches (first %) (second %) 0))
       (reduce +)))
