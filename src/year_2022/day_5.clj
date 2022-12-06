(ns year-2022.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn move [stacks instruction part]
  (let [[n from to] (->> (re-seq #"move (\d+) from (\d+) to (\d+)"
                                 instruction)
                         first
                         rest
                         (map parse-long))
        from (dec from)
        to (dec to)
        grabbed (cond-> (take n (stacks from))
                  (part #{:part-2}) reverse)]
    (-> (update stacks to #(apply conj % grabbed))
        (update from #(drop n %)))))

(defn parse [input]
  (->> (str/split-lines input)
       (apply map list)
       (map #(filter (fn [c] (<= 65 (int c) 90)) %))
       (filter seq)
       (into [])))

(->> (slurp (io/resource "2022/5/input"))
     str/split-lines
     (reduce #(move %1 %2 :part-2) (parse (slurp (io/resource "2022/5/header"))))
     (map first)
     (apply str))
