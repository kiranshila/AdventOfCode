(ns year-2020.day-3-test
  (:require [year-2020.day-3 :as sut]
            [clojure.test :refer :all]))

(def test-str
"..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(deftest given-cases-1
  (is (= 7 (sut/solution-1 test-str))))

(deftest given-cases-2
  (is (= 336 (sut/solution-2 test-str))))
