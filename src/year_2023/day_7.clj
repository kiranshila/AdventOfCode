(ns year-2023.day-7
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.core.match :refer [match]]))

(def input (slurp (io/resource "2023/7/input")))
(def example-input (slurp (io/resource "2023/7/example")))

(defn rank [hand-str]
  (let [freq (frequencies hand-str)
        unique (count freq)
        matches (apply max (vals freq))]
    (match [unique matches]
      [1 _] 6
      [2 4] 5
      [2 3] 4
      [3 3] 3
      [3 2] 2
      [4 _] 1
      [5 _] 0)))

(def strength {\A 13 \K 12 \Q 11 \J 10 \T 9 \9 8 \8 7 \7 6 \6 5 \5 4 \4 3 \3 2 \2 1})

(defn comp-hand [rank strength a b]
  (let [c (compare (rank a) (rank b))]
    (if (zero? c)
      (compare
       (->> a (mapv strength))
       (->> b (mapv strength)))
      c)))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #"\s+"))))

(defn solve [input cmp]
  (->> (parse-input input)
       (sort-by first cmp)
       (map second)
       (map parse-long)
       (map-indexed vector)
       (map (fn [[i v]] (* (inc i) v)))
       (reduce +)))

(defn rank-2 [hand-str]
  (let [freq (frequencies hand-str)
        unique (count freq)
        matches (apply max (vals freq))
        js (get freq \J 0)]
    (match [unique matches js]
       ; five of a kind
      [1 _ _] 6
      ; normal four of a kind
      [2 4 0] 5
      ; four of a kind, but the four are jokers -> five of a kind
      [2 4 4] 6
      ; four of a kind, but the 1 card is a joker -> five of a kind
      [2 4 1] 6
      ; Normal full house
      [2 3 0] 4
      ; Full house but the three cards are jokers -> five of a kind
      [2 3 3] 6
      ; Full house but the other two are jokers -> five of a kind
      [2 3 2] 6
      ; Normal three of a kind
      [3 3 0] 3
      ; Three of a kind with three jokers -> four of a kind
      [3 3 3] 5
      ; Three of a kind with one joker -> four of a kind
      [3 3 1] 5
      ; Normal two pair
      [3 2 0] 2
      ; two pair with one joker -> full house
      [3 2 1] 4
      ; two pair, with one pair being jokers -> four of a kind
      [3 2 2] 5
      ; Normal one pair
      [4 _ 0] 1
      ; one pair with a pair of jokers -> three of a kind
      [4 _ 2] 3
      ; one pair with one joker -> three of a kind
      [4 _ 1] 3
      ; Normal high card
      [5 _ 0] 0
      ; High card with a joker -> one pair
      [5 _ 1] 1)))

;; Part 1
(solve input (partial comp-hand rank strength))
;; Part 2
(solve input (partial comp-hand rank-2 (assoc strength \J 0)))
