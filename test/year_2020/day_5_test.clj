(ns year-2020.day-5-test
  (:require [year-2020.day-5 :as sut]
            [clojure.test :refer [deftest is]]))

(deftest given-cases-1
  (is (= 567 (sut/solution-1 "BFFFBBFRRR")))
  (is (= 119 (sut/solution-1 "FFFBBBFRRR")))
  (is (= 820 (sut/solution-1 "BBFFBBFRLL"))))
