(ns year-2023.day-3
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (slurp (io/resource "2023/3/input")))

(defn parse-line [line]
  (loop [idx 0
         numbers '()
         symbols '()
         num ""
         rem line]
    (if (not (seq rem))
      {:numbers (into {} numbers) :symbols (into {} symbols)}
      (let [char (first rem)]
        (if (Character/isDigit char)
          (recur (inc idx) numbers symbols (str num char) (rest rem))
          (let [next-nums (if (seq num) (conj numbers [(dec idx) num]) numbers)]
            (if (= \. char)
              (recur (inc idx) next-nums symbols "" (rest rem))
              (recur (inc idx) next-nums (conj symbols [idx char]) "" (rest rem)))))))))

(defn neighbors [width idx]
  [(dec (- idx width)) (- idx width) (inc (- idx width))
   (dec idx)                         (inc idx)
   (dec (+ idx width)) (+ idx width) (inc (+ idx width))])

(defn part-number? [idx num symbols width]
  (let [all-check-idxs (range (inc (- idx (count num))) (inc idx))
        all-neighbors (->> (map (partial neighbors width) all-check-idxs)
                           flatten
                           (filter nat-int?)
                           (into #{})
                           (#(apply disj % all-check-idxs)))]
    (not-every? nil? (map symbols all-neighbors))))

(defn part-1 [input]
  (let [width (str/index-of input "\n")
        parsed (parse-line (str/replace input #"\n" ""))]
    (->> (for [[idx num] (:numbers parsed)
               :when (part-number? idx num (:symbols parsed) width)]
           num)
         (map parse-long)
         (reduce +))))

(defn expand-numbers
  "Map from pixel index to number index (vals)"
  [numbers]
  (into {} (for [[i [idx num]] (map-indexed vector numbers)
                 idx (range (inc (- idx (count num))) (inc idx))]
             [idx i])))

(defn gear-ratio [expanded-nums nums width idx]
  (let [all-neighbors (->> (neighbors width idx)
                           (filter nat-int?)
                           (into #{}))
        touches (->> (map expanded-nums all-neighbors)
                     (filter some?)
                     (into #{}))]
    (when (= 2 (count touches))
      (* (get nums (first touches))
         (get nums (second touches))))))

(defn part-2 [input]
  (let [width (str/index-of input "\n")
        parsed (parse-line (str/replace input #"\n" ""))
        possible-gears (->> (filter (fn [[_ v]] (= v \*)) (:symbols parsed))
                            (map first))
        nums (mapv parse-long (vals (:numbers parsed)))
        e-n (expand-numbers (:numbers parsed))]
    (->> (map (partial gear-ratio e-n nums width) possible-gears)
         (filter some?)
         (reduce +))))
