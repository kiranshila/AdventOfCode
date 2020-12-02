(ns year-2020.day-2-test
  (:require [year-2020.day-2 :as sut]
            [clojure.test :refer :all]))

(def test-str "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc")

(deftest given-cases-1
  (is (= 2 (sut/solution-1 test-str))))

(deftest given-cases-2
  (is (= 1 (sut/solution-2 test-str))))
