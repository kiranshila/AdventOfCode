(ns year-2020.day-1-test
  (:require [year-2020.day-1 :as sut]
            [clojure.test :refer :all]))

(deftest given-cases-1
  (is (= 514579 (sut/solution-1 [1721
                                 979
                                 366
                                 299
                                 675
                                 1456]
                                2020))))

(deftest given-cases-2
  (is (= 241861950 (sut/solution-2 [1721
                                    979
                                    366
                                    299
                                    675
                                    1456]
                                   2020))))
