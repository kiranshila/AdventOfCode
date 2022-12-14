(ns year-2022.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn find-in [graph value]
  (->> (range (count graph))
       (map #(map first (filter (comp (partial = value) second)
                                (map-indexed vector (graph %)))))
       (map-indexed vector)
       (filter (comp seq second))
       (mapcat (fn [[r cs]] (map (partial vector r) cs)))))

(def pre-graph
  (->> (slurp (io/resource "2022/12/input"))
       str/split-lines
       (map (partial mapv #(- (int %) 97)))
       vec))
(def start (first (find-in pre-graph -14)))
(def stop (first (find-in pre-graph -28)))
(def graph (-> graph (assoc-in start 0) (assoc-in stop 25)))

(defn neighbors [point graph explored]
  (->> (map (partial mapv + point) [[1 0] [-1 0] [0 1] [0 -1]])
       (filter #(and (<= 0 (first %) (dec (count graph)))
                     (<= 0  (second %) (dec (count (graph 0))))
                     (<= (get-in graph %) (inc (get-in graph point)))
                     (not (contains? explored %))))))

(defn solve [start stop graph]
  (loop [explored #{start}
         to-explore (conj (clojure.lang.PersistentQueue/EMPTY) start)
         parents {}]
    (when (seq to-explore)
      (let [current (first to-explore)]
        (if (= current stop)
          (take-while some? (iterate parents stop))
          (let [ns (neighbors current graph explored)]
            (recur (into explored ns)
                   (into (pop to-explore) ns)
                   (reduce #(assoc %1 %2 current) parents ns))))))))

(def part-1
  (->> (solve start stop graph)
       count
       dec))

(def part-2
  (let [starts (find-in graph 0)]
    (->> (map #(solve % stop graph) starts)
         (filter identity)
         (map (comp dec count))
         (apply min))))
