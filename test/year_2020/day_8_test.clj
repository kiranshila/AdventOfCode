(ns year-2020.day-8-test
  (:require [year-2020.day-8 :as sut]
            [clojure.test :refer [deftest is]]))

(def test-str "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")


(deftest given-cases-1
  (is (= 0 (sut/solution-1 test-str))))

(deftest given-cases-2
  (is (= 0 (sut/solution-2 test-str))))
