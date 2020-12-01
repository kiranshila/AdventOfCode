(ns year-2017.day-1-test
  (:require [year-2017.day-1 :as sut]
            [clojure.test :refer :all]))

(deftest given-cases-1
  (is (= 3 (sut/solution-1 "1122")))
  (is (= 4 (sut/solution-1 "1111")))
  (is (= 0 (sut/solution-1 "1234")))
  (is (= 9 (sut/solution-1 "91212129"))))

(deftest given-cases-2
  (is (= 6 (sut/solution-2 "1212")))
  (is (= 0 (sut/solution-2 "1221")))
  (is (= 4 (sut/solution-2 "123425")))
  (is (= 12 (sut/solution-2 "123123")))
  (is (= 4 (sut/solution-2 "12131415"))))
