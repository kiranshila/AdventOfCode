(ns year-2020.day-7
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def input (slurp (io/resource "2020/7/input")))

(def grammar
  (insta/parser "
<Rule> = ColoredBag <Space> <Contain> <Space> (Bags|NoBags)
<ColoredBag> = Color <Space> <Bag>
Color = #'\\w+ \\w+'
Bag = #'bag[s]?'
Space = #' '
Contain = <#'contain'>
Bags = Contained+
Contained = Count <Space> ColoredBag <Punctuation>
Count = #'\\d'
Punctuation = #'(,\\s)|\\.?'
NoBags = <#'no other bags.'>"))

(defn transform [input]
  (let [ast (grammar input)]
    (apply hash-map (insta/transform
                     {:Count edn/read-string
                      :Color #(keyword (str/replace % #" " "-"))
                      :Contained #(hash-map %2 %1)
                      :Bags merge
                      :NoBags #(hash-map)} ast))))

(defn transpose-tree [tree]
  (apply merge-with concat (for [color (keys tree)]
     (apply merge-with conj (for [child (keys (color tree))]
                              {child (list color)})))))

(defn traverse-color [tree color]
  (loop [need-to-visit [color]
         visited #{}]
    (if (empty? need-to-visit)
      visited
      (let [color (peek need-to-visit)
            contains (color tree)
            need-to-visit (into (pop need-to-visit) contains)]
        (if (contains? visited color)
          (recur need-to-visit visited)
          (recur need-to-visit (conj visited color)))))))

(defn traverse-color-2 [tree color]
  (loop [need-to-visit [[color 1]]
         count 0]
    (if (empty? need-to-visit)
      count
      (let [[color mult] (peek need-to-visit)
            contains (color tree)
            need-to-visit (into (pop need-to-visit) (map (fn [[k v]] [k (* v mult)])contains))]
        (recur need-to-visit (+ count mult))))))

(defn solution-1 [input]
  (let [rules (str/split-lines input)
        tree (->> rules
                  (map transform)
                  (reduce merge))]
    (let [transpose-tree (transpose-tree tree)]
      (dec (count (traverse-color transpose-tree :shiny-gold))))))

(defn solution-2 [input]
  (let [rules (str/split-lines input)
        tree (->> rules
                  (map transform)
                  (reduce merge))]
    (dec (traverse-color-2 tree :shiny-gold))))
