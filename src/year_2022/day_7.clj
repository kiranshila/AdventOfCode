(ns year-2022.day-7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn size [line])

(->> (slurp (io/resource "2022/7/input"))
     str/split-lines)
