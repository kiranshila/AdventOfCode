(ns year-2020.day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (slurp (io/resource "2020/14/input")))

(defn big-bin [str]
  (new java.math.BigInteger str 2))

; stole this. Clojure numeric stack not lookin so hot right now
(defn big-or
  [f & r]
  (reduce (fn [acc v] (.or acc (biginteger v))) (biginteger f) r))

(defn big-and
  [f & r]
  (reduce (fn [acc v] (.and acc (biginteger v))) (biginteger f) r))

(defn apply-mask [mask value]
  (let [and-mask (-> mask
                     (str/replace #"[10]" "0")
                     (str/replace #"X" "1")
                     big-bin)
        or-mask (-> mask
                    (str/replace #"X" "0")
                    big-bin)]
    (big-or (big-and and-mask value) or-mask)))

(defn solution-1 [input]
  (loop [lines (str/split-lines input)
         memory {}
         mask nil]
    (if (empty? lines)
      (reduce + (vals memory))
      (let [line (first lines)]
        (if (str/starts-with? line "mask")
          (recur (rest lines) memory  (-> line
                                          (str/split #"=")
                                          second
                                          str/trim))
          (let [[_ addr value] (first (re-seq #"mem\[(\d+)\] = (\d+)" line))
                addr (edn/read-string addr)
                value (edn/read-string value)]
            (recur (rest lines) (conj memory [addr (apply-mask mask value)]) mask)))))))

(defn bit-pos [num pos]
  (if (< 0 (bit-and num (bit-shift-left 1 pos)))
    "1"
    "0"))

(defn get-addrs [mask addr]
  (for [num (range (Math/pow 2 (count (filter (partial = \X) mask))))]
    (loop [n 0
           final-str ""
           s mask]
      (if (= "" s)
        (big-or (big-bin final-str)
                (big-and (-> mask
                             (str/replace #"[10]" "1")
                             (str/replace #"X" "0")
                             big-bin)
                         addr))
        (if (= (first s) \X)
          (recur (inc n) (str final-str (bit-pos num n)) (apply str (rest s)))
          (recur n (str final-str (first s)) (apply str (rest s))))))))

(defn solution-2 [input]
  (loop [lines (str/split-lines input)
         memory {}
         mask nil]
    (if (empty? lines)
      (reduce + (vals memory))
      (let [line (first lines)]
        (if (str/starts-with? line "mask")
          (recur (rest lines) memory  (-> line
                                          (str/split #"=")
                                          second
                                          str/trim))
          (let [[_ addr value] (first (re-seq #"mem\[(\d+)\] = (\d+)" line))
                addr (edn/read-string addr)
                value (edn/read-string value)]
            (recur (rest lines) (apply conj memory (for [addr (get-addrs mask addr)]
                                   [addr value])) mask)))))))
