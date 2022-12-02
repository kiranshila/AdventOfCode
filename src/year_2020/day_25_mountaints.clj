(ns year-2020.day-25-mountaints)

(def card-public 18499292)
(def door-public 8790390)

(defn transform [subject-number value]
  (rem (* value subject-number) 20201227))

(defn crack [public]
  (let [subject-number 7]
    (loop [value 1, n 0]
     (if (= value public)
       n
       (recur (transform subject-number value) (inc n))))))

(defn solution [card-public door-public]
  (nth (iterate (partial transform door-public) 1) (crack card-public)))
