(ns year-2023.day-4
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.math :as math]
   [clojure.java.io :as io]))

(def input (slurp (io/resource "2023/4/input")))

(defn parse-nums [nums]
  (->> (str/split nums #" ")
       (filter seq)
       (map parse-long)
       (into #{})))

(defn parse-card [card]
  (let [[num rem] (str/split card #":")
        id (parse-long (str/trim (second (str/split num #"Card "))))
        [winning have] (str/split rem #"\|")]
    {:id id
     :winning (parse-nums winning)
     :have (parse-nums have)}))

(defn score-card [{:keys [winning have]}]
  (let [have-winning (set/intersection winning have)
        winning (count have-winning)]
    (if (zero? winning) 0 (math/pow 2 (dec (count have-winning))))))

(defn part-1 [input]
  (->> (str/split-lines input)
       (map parse-card)
       (map score-card)
       (reduce +)))

(defn part-2 [input]
  (let [parsed (map parse-card (str/split-lines input))]
    (loop [card-ids {}
           cards parsed]
      (if (not (seq cards))
        (reduce + (vals card-ids))
        (let [{:keys [id winning have]} (first cards)
              card-ids (update card-ids id (fnil inc 0)) ; Add the original
              copies (count (set/intersection winning have))
              to-add (range (inc id) (inc (+ id copies)))]
          (recur (reduce (fn [ids next-id]
                           (update ids next-id #((fnil + 0) % (card-ids id))))
                         card-ids to-add)
                 (rest cards)))))))
