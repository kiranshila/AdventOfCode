(ns year-2020.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :refer [keywordize-keys]]))

(def input (slurp (io/resource "2020/4/input")))

(defn strint-in-bounds [strint min max]
  (let [num (Integer/parseInt strint)]
    (and (>= num min) (<= num max))))

(def rules {:byr #(strint-in-bounds % 1920 2002)
            :iyr #(strint-in-bounds % 2010 2020)
            :eyr #(strint-in-bounds % 2020 2030)
            :pid #(some? (re-matches #"^\d{9}$" %))
            :ecl #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %)
            :hcl #(some? (re-matches #"^#[0-9a-f]{6}" %))
            :hgt #(when-some [[_ height unit] (re-matches #"(\d+)(cm|in)" %)]
                    (case unit
                      "cm" (strint-in-bounds height 150 193)
                      "in" (strint-in-bounds height 59 76)))})

(defn parse [line]
  (dissoc (keywordize-keys
           (apply hash-map
                  (-> line
                      (str/replace #":|\n" " ")
                      (str/split #" "))))
          :cid))

(defn valid-fields? [passport]
  (let [rules-keys (set (keys rules))
        keys (set (keys passport))]
    (every? keys rules-keys)))

(defn valid? [passport]
  (and
   (valid-fields? passport)
   (every? identity (map (fn [[key value]] ((rules key) value)) passport))))

(defn solution-1 [input]
  (let [passports (map parse (str/split input #"\n\n"))]
    (count (filter valid-fields? passports))))

(defn solution-2 [input]
  (let [passports (map parse (str/split input #"\n\n"))]
    (count (filter valid? passports))))
