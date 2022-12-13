(ns year-2022.day-11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-monkey [input]
  (let [params (->> input
                    str/split-lines
                    rest)
        [lhs op rhs] (-> (subs (nth params 1) 19)
                         (str/replace #"old" "%")
                         (str/split #" "))]
    {:item-worry-levels (vec (->> #","
                                  (str/split (subs (nth params 0) 18))
                                  (map str/trim)
                                  (map parse-long)))
     :op (eval (read-string (str "#(" op " " lhs " " rhs ")")))
     :test {:div (parse-long (subs (nth params 2) 21))
            :true (parse-long (subs (nth params 3) 29))
            :false (parse-long (subs (nth params 4) 30))}
     :items-inspected 0}))

(def monkeys (->> #"\n\n"
                  (str/split (slurp (io/resource "2022/11/input")))
                  (map parse-monkey)
                  vec))

(defn turn [inspect-fn monkeys n]
  (let [{:keys [item-worry-levels op test]} (nth monkeys n)]
    (-> (reduce
         (fn [monkeys wl]
           (let [wl (inspect-fn (op wl))]
             (if (= 0 (mod wl (:div test)))
               (update-in monkeys [(:true test) :item-worry-levels] conj wl)
               (update-in monkeys [(:false test) :item-worry-levels] conj wl))))
         monkeys item-worry-levels)
        (assoc-in [n :item-worry-levels] [])
        (update-in [n :items-inspected] + (count item-worry-levels)))))

(defn round [turn-fn monkeys]
  (reduce turn-fn monkeys (range (count monkeys))))

(def part-1
  (->> (iterate (partial round (partial turn #(quot % 3))) monkeys)
       (#(nth % 20))
       (map :items-inspected)
       sort
       (take-last 2)
       (apply *)))

(def big-boi (apply * (map #(get-in % [:test :div]) monkeys)))

(def part-2
  (->> (iterate (partial round (partial turn #(- % (* big-boi (quot % big-boi))))) monkeys)
       (#(nth % 10000))
       (map :items-inspected)
       sort
       (take-last 2)
       (apply *)))
