(ns year-2020.day-12-test
  (:require [year-2020.day-12 :as sut]
            [clojure.test :refer [deftest is]]))

(def test-str "")

(deftest given-cases-1
  (is (= 35 (sut/solution-1 test-str))))

(deftest given-cases-2
  (is (= 8 (sut/solution-2 test-str))))
