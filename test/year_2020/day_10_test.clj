(ns year-2020.day-11-test
  (:require [year-2020.day-11 :as sut]
            [clojure.test :refer [deftest is]]))

(def test-str "")

(deftest given-cases-1
  (is (= 37 (sut/solution-1 test-str))))

(deftest given-cases-2
  (is (= 26 (sut/solution-2 test-str))))
