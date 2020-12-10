(ns year-2020.day-10-test
  (:require [year-2020.day-10 :as sut]
            [clojure.test :refer [deftest is]]))

(def test-str-one "16
10
15
5
1
11
7
19
6
12
4")

(def test-str-two "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(deftest given-cases-1
  (is (= 35 (sut/solution-1 test-str-one)))
  (is (= 220 (sut/solution-1 test-str-two))))

(deftest given-cases-2
  (is (= 8 (sut/solution-2 test-str-one)))
  (is (= 19208 (sut/solution-2 test-str-two))))
