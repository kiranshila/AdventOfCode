(ns year-2022.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-pair [pair-str]
  (->> pair-str
       str/split-lines
       (#(map (comp read-string eval) %))))

(def input
  (->> (slurp (io/resource "2022/13/input"))
       (#(str/split % #"\n\n"))
       (map parse-pair)))

(defn comp-fn [l r]
  (cond
    (and (nil? l)        r)              -1
    (and l               (nil? r))        1
    (and (int? l)        (int? r))        (compare l r)
    (and (int? l)        (sequential? r)) (recur (vector l) r)
    (and (sequential? l) (int? r))        (recur l (vector r))
    (and (empty? l)      (empty? r))      0
    (and (sequential? l) (sequential? r)) (let [c (comp-fn (first l) (first r))]
                                            (if (= c 0)
                                              (recur (next l) (next r))
                                              c))))

(def part-1
  (->> (map #(apply comp-fn %) input)
       (map-indexed vector)
       (filter #(< (second %) 1))
       (map #(update % 0 inc))
       (map first)
       (reduce +)))

(def part-2
  (->> (sort comp-fn (mapcat (partial sort comp-fn)
                             (conj input (list [[2]] [[6]]))))
       (map-indexed vector)
       (filter #(or (= (second %) [[2]])
                    (= (second %) [[6]])))
       (map first)
       (map inc)
       (reduce *)))
