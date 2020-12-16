(ns year-2020.day-16
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def input (slurp (io/resource "2020/16/input")))

(defn parse-rule [rule-line]
  (let [[rule gt1 lt1 gt2 lt2] (->> rule-line
                                    (re-seq #"([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)")
                                    first
                                    rest)
        [gt1 lt1 gt2 lt2] (map edn/read-string [gt1 lt1 gt2 lt2])]
    [(keyword (str/replace rule #" " "-"))
     [[gt1 lt1] [gt2 lt2]]]))

(defn parse-ticket [ticket-line]
  (as-> ticket-line x
    (str/split x #",")
    (map edn/read-string x)))

(defn valid? [rule num]
  (let [[_ [[gt1 lt1] [gt2 lt2]]] rule]
    (or (<= gt1 num lt1)
        (<= gt2 num lt2))))

(defn valid-rules [rules num]
  (for [rule rules
        :when (valid? rule num)]
    (first rule)))

(defn invalid-entries [ticket rules]
  (for [num ticket
        :when (every? not (for [rule rules]
                            (valid? rule num)))]
    num))

(def departures [:departure-date :departure-location :departure-platform
                 :departure-station :departure-time :departure-track])

(defn solution-1 [input]
  (let [[rules _ nearby-tickets] (str/split input #"\n\n")
        rules (map parse-rule (str/split-lines rules))
        nearby-tickets (map parse-ticket (rest (str/split-lines nearby-tickets)))]
    (reduce + (flatten (for [ticket nearby-tickets]
                         (invalid-entries ticket rules))))))

(defn solution-2 [input]
  (let [[rules your-ticket nearby-tickets] (str/split input #"\n\n")
        your-ticket (parse-ticket (second (str/split-lines your-ticket)))
        rules (map parse-rule (str/split-lines rules))
        nearby-tickets (map parse-ticket (rest (str/split-lines nearby-tickets)))]
    (->> (for [ticket nearby-tickets
               :when (not (seq (invalid-entries ticket rules)))]
           (map (partial valid-rules rules) ticket))
         (apply map vector)
         (map #(map set %))
         (map #(apply set/intersection %))
         ((fn [x]
            (map (fn [y]
                   (apply set/difference
                          y
                          (filter #(< (count %) (count y)) x)))
                 x)))
         (map first)
         (#(map hash-map  % your-ticket))
         (apply merge)
         (#(select-keys % departures ))
         vals
         (reduce *))))
