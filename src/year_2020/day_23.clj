(ns year-2020.day-23)

(def cups [7 9 2 8 4 5 1 3 6])

(defn build-forward-pointers [cups]
  (into {} (map vector cups (rest (cycle cups)))))

(defn find-destination [current forward not-these minimum maximum]
  (loop [current current]
    (if (< current minimum)
      (recur maximum)
      (if (and (contains? forward current)
               (not (contains? not-these current)))
        current
        (recur (dec current))))))

(defn crab-game [forward current target]
  (let [minimum (apply min (keys forward))
        maximum (apply max (keys forward))]
    (loop [move 0
           current current
           forward (transient forward)]
      (if (= move target)
        (persistent! forward)
        (let [head (forward current)
              middle (forward head)
              tail (forward middle)
              destination (find-destination (dec current)
                                            forward
                                            #{head middle tail}
                                            minimum
                                            maximum)]
          (recur (inc move)
                 (forward tail)
                 (conj! forward {current (forward tail)
                                 destination (forward current)
                                 tail (forward destination)})))))))

(defn solution-1 [cups]
  (let [forward (build-forward-pointers cups)]
    (crab-game forward (first cups) 100)))

(defn solution-2 [cups]
  (let [forward (build-forward-pointers (apply conj cups (range (inc (apply max cups)) 1000001)))
        result (crab-game forward (first cups) 10000000)
        label-1 (result 1)
        label-2 (result label-1)]
    (* label-1 label-2)))
