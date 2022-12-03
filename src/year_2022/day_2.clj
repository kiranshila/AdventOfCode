(ns year-2022.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def op-move {"A" :r "B" :p "C" :s})
(def my-move {"X" :r "Y" :p "Z" :s})
(def points {:r 1 :p 2 :s 3})
(def beats {:r :s , :s :p , :p :r})

(defn play [op you]
  (+ (points you)
     (condp = you
       (beats op) 0
       op 3
       (beats (beats op)) 6)))

(defn go [line]
  (let [[l r] (str/split line #" ")]
    (play (op-move l) (my-move r))))

;; Part 2

(defn go-2 [line]
  (let [[op you] (str/split line #" ")
        op (op-move op)]
    (play
     op
     (case you
       "X" (beats op)
       "Y" op
       "Z" (beats (beats op))))))

(->> (slurp (io/resource "2022/2/input"))
     (str/split-lines)
     (map go)
     #_(map go-2)
     (reduce +))
