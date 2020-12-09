(ns year-2020.day-9-test
  (:require [year-2020.day-9 :as sut]
            [clojure.test :refer [deftest is]]))

(def test-str "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(deftest given-cases-1
  (is (= 127 (sut/solution-1 test-str 5))))

(deftest given-cases-2
  (is (= 62 (sut/solution-2 test-str 5))))
