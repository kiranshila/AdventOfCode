(ns year-2020.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (slurp (io/resource "2020/8/input")))

(def other {:nop :jmp
            :jmp :nop})

(defn decode [input]
  (let [[op arg] (rest (first (re-seq #"(\w+) ([\+|-]\d+)" input)))
        op (keyword op)
        arg (edn/read-string arg)]
    [op arg]))

(defn run [instructions]
  (loop [instructions instructions
         pc 0, acc 0, hist #{}]
    (if (contains? hist pc)
      {:loop acc} ;loop
      (if (= pc (count instructions))
        {:end acc} ;normal termination
        (let [[op arg] (nth instructions pc)]
          (case op
            :jmp (recur instructions (+ pc arg) acc (conj hist pc))
            :acc (recur instructions (inc pc) (+ acc arg) (conj hist pc))
            :nop (recur instructions (inc pc) acc (conj hist pc))))))))

(defn solution-1 [input]
  (let [instructions (into [] (map decode (str/split-lines input)))]
    (run instructions)))

(defn solution-2 [input]
  (let [instructions (into [] (map decode (str/split-lines input)))]
    (first (for [i (range (count instructions))
                 op (nth instructions i)
                 :when (or (= op :nop)
                           (= op :jmp))
                 :let [instructions (update-in instructions [i 0] other)
                       break (run instructions)]
                 :when (contains? break :end)]
             break))))
