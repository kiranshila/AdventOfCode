(ns year-2020.day-13-test
  (:require [year-2020.day-13 :as sut]
            [clojure.test :refer [deftest is]]))

(def test-str "939
7,13,x,x,59,x,31,19")

(deftest given-cases-1
  (is (= 295 (sut/solution-1 test-str))))

(deftest given-cases-2
  (is (= 1068781 (sut/solution-2 test-str))))
