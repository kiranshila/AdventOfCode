(ns year-2022.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Why parse when I can type
(def stacks
  ['(:P :V :Z :W :D :T)
   '(:D :J :F :V :W :S :L)
   '(:H :B :T :V :S :L :M :Z)
   '(:J :S :R)
   '(:W :L :M :F :G :B :Z :C)
   '(:B :G :R :Z :H :V :W :Q)
   '(:N :D :B :C :P :J :V)
   '(:Q :B :T :P)
   '(:C :R :Z :G :H)])

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

(->> (slurp (io/resource "2022/5/input"))
     str/split-lines
     (reduce #(move %1 %2 :part-2) stacks)
     (map first)
     (map name)
     (apply str))
