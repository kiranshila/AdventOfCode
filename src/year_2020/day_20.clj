(ns year-2020.day-20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.matrix :as mat]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def input (slurp (io/resource "2020/20/input")))

(def tile-size 10)

(defn parse-tile [tile-str]
  (let [lines (str/split-lines tile-str)
        id (edn/read-string (->> (first lines)
                                 (re-seq #"Tile (\d+):")
                                 first
                                 second))]
    {id (into [] (for [row (rest lines)]
                   (into [] (for [char row]
                              (case char
                                \# 1
                                \. 0)))))}))

(def side-pairs {:N :S
                 :S :N
                 :E :W
                 :W :E})

(defn get-edge [tile dir]
  (case dir
    :N (mat/get-row tile 0)
    :S (mat/get-row tile (dec tile-size))
    :E (mat/get-column tile (dec tile-size))
    :W (mat/get-column tile 0)))

(defn flip-lr [m]
  (into [] (map #(into [] (reverse %)) m)))

(defn flip-ud [m]
  (into [] (reverse m)))

(def transformations (range 8))

(defmulti transform (fn [_ trans] trans))
(defmethod transform 0 [m _] m)
(defmethod transform 1 [m _] (mat/transpose (flip-lr m)))
(defmethod transform 2 [m _] (flip-ud (flip-lr m)))
(defmethod transform 3 [m _] (flip-lr (mat/transpose m)))
(defmethod transform 4 [m _] (flip-lr m))
(defmethod transform 5 [m _] (mat/transpose m))
(defmethod transform 6 [m _] (flip-ud m))
(defmethod transform 7 [m _] (flip-lr (mat/transpose (flip-lr m))))

(defn replace-keys [replacements m]
  (reduce-kv #(assoc %1 (replacements %2) %3) {} m))

(defn transform-solution [m trans]
  (replace-keys
   (case trans
     0 {:N :N :S :S :E :E :W :W}
     1 {:N :W :S :E :E :N :W :S}
     2 {:N :S :S :N :E :W :W :E}
     3 {:N :E :S :W :E :S :W :N}
     4 {:N :N :S :S :E :W :W :E}
     5 {:N :W :S :E :E :S :W :N}
     6 {:N :S :S :N :E :E :W :W}
     7 {:N :E :S :W :E :N :W :S})
   m))

(defn find-matching-edge [edge side other-tiles]
  (first (for [[id tile] other-tiles
               trans transformations
               :let [tile-trans (transform tile trans)]
               :when (= edge (get-edge tile-trans side))]
           {:id id :trans trans})))

(defn find-matching-tiles [tile other-tiles]
  (apply merge (for [side (keys side-pairs)
                     :let [edge (get-edge tile side)]]
                 {side (find-matching-edge edge (side-pairs side) other-tiles)})))

(defn solve-jigsaw [tiles]
  (apply merge (for [[id tile] tiles
                     :let [other-tiles (dissoc tiles id)]]
                 {id (find-matching-tiles tile other-tiles)})))

(defn solution-1 [input]
  (let [tile-strs (str/split input #"\n\n")
        tiles (->> tile-strs
                   (map parse-tile)
                   (apply merge))]
    (->> (solve-jigsaw tiles)
         ; Find corner pieces
         (map (fn [[k v]] [k (->> (vals v)
                                  (filter nil?)
                                  count)]))
         (filter #(= 2 (second %)))
         ; Multiply IDs
         (map first)
         (reduce *))))

(defn is-top-left? [[_ edge-matches]]
  (and (= (:N edge-matches) nil)
       (= (:W edge-matches) nil)))

(defn find-top-left [solution-map]
  (first (first (for [tile solution-map
                      :when (is-top-left? tile)]
                  tile))))

(defn build-row-map [solution-map first-id first-trans]
  (loop [row [[first-id first-trans]]]
    (if (nil? (first (last row)))
      (into [] (butlast row))
      (let [[prev-id prev-trans] (last row)
            [next-id next-trans] (->> (solution-map prev-id)
                                      (#(reduce transform-solution % (reverse prev-trans)))
                                      :E
                                      vals)]
        (recur (conj row [next-id (conj prev-trans next-trans)]))))))

(defn build-image-map [solution-map]
  (loop [rows [(build-row-map solution-map (find-top-left solution-map) [0])]]
    (let [[prev-id prev-trans] (first (last rows))
          [next-first-id next-first-trans] (->> (solution-map prev-id)
                                                (#(reduce transform-solution % (reverse prev-trans)))
                                                :S
                                                vals)]
      (if (nil? next-first-id)
        rows
        (recur (conj rows (build-row-map solution-map next-first-id (conj prev-trans next-first-trans))))))))

(defn hcat [mats]
  (vec (apply map (comp vec concat) mats)))

(defn vcat [mats]
  (vec (apply concat mats)))

(defn flatten-image [nested-image]
  (vcat (map hcat nested-image)))

(defn submat-center [m]
  (let [[xsize _] (mat/shape m)
        subsize (dec xsize)]
    (mat/select m (range 1 subsize) (range 1 subsize))))

(defn build-image [solution-map tiles]
  (let [image-map (build-image-map solution-map)
        [gridx gridy _] (mat/shape image-map)
        nested-image (for [i (range gridx)]
                       (for [j (range gridy)
                             :let [[id trans] (get-in image-map [i j])]]
                         (submat-center (reduce transform (tiles id) (reverse trans)))))]
    (flatten-image nested-image)))

(def sea-monster
  [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0]
   [1 0 0 0 0 1 1 0 0 0 0 1 1 0 0 0 0 1 1 1]
   [0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 0]])

(def sea-monster-hashes 15)

(defn find-sea-monster [image]
  (let [[sm-y sm-x] (mat/shape sea-monster)
        [image-x image-y] (mat/shape image)]
    (for [x (range (- image-x sm-x))
          y (range (- image-y sm-y))
          :let [scan-region (mat/select image (range y (+ y sm-y)) (range x (+ x sm-x)))]
          :when (= sea-monster (mat/emap bit-and scan-region sea-monster))]
      [x y])))

(defn solution-2 [input]
  (let [tile-strs (str/split input #"\n\n")
        tiles (->> tile-strs
                   (map parse-tile)
                   (apply merge))
        solution-map (solve-jigsaw tiles)
        image (build-image solution-map tiles)]
    (->> (for [trans (range 8)
           :let [monsters (find-sea-monster (transform image trans))]
           :when monsters]
           (count monsters))
         (filter (partial not= 0))
         first
         (* sea-monster-hashes)
         (- (count (filter (partial = 1) (flatten image)))))))
