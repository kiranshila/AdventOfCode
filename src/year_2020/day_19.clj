(ns year-2020.day-19
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [clojure.string :as str]))

(def input (slurp (io/resource "2020/19/input")))

(defn solution [input]
  (let [[rules messages] (str/split input #"\n\n")
        parser (insta/parser rules)
        messages (str/split-lines messages)]
    (->> messages
         (map #(parser % :start :0))
         (filter #(not (insta/failure? %)))
         count)))
