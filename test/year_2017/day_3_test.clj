(ns year-2017.day-3-test
  (:require [year-2017.day-3 :as sut]
            [clojure.test :as t :refer [deftest is]]))

(deftest given-cases-1
  (is (== 0 (sut/solution-1 1)))
  (is (== 3 (sut/solution-1 12)))
  (is (== 2 (sut/solution-1 23)))
  (is (== 31 (sut/solution-1 1024))))
