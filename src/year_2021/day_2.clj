(ns year-2021.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "2021/2/input"))
                str/split-lines
                (map #(str/split % #" "))))

(defn pt1 [[depth forward] instruction]
  (let [dir (first instruction)
        amnt (Integer/parseInt (second instruction))]
    (case dir
      "forward" [depth (+ amnt forward)]
      "up" [(- depth amnt) forward]
      "down"  [(+ depth amnt) forward])))

(apply * (reduce pt1 [0 0] input))

(defn pt2 [[aim depth forward] instruction]
  (let [dir (first instruction)
        amnt (Integer/parseInt (second instruction))]
    (case dir
      "forward" [aim (+ depth (* aim amnt)) (+ amnt forward)]
      "up" [(- aim amnt) depth forward]
      "down"  [(+ aim amnt) depth forward])))

(apply * (rest (reduce pt2 [0 0 0] input)))
