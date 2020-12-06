(ns year-2020.day-6-test
  (:require [year-2020.day-6 :as sut]
            [clojure.test :refer [deftest is]]))

(def test-str "abc

a
b
c

ab
ac

a
a
a
a

b")

(deftest given-cases-1
  (is (= 11 (sut/solution-1 test-str))))

(deftest given-cases-2
  (is (= 6 (sut/solution-2 test-str))))
