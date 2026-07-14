(ns jolt.time.year-test
  "Stage 3c gate: Year, YearMonth. Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.year])
  (:import [java.time Year YearMonth]))

(deftest year
  (is (= 2020 (.getValue (Year/of 2020))))
  (is (.isLeap (Year/of 2020)))
  (is (not (.isLeap (Year/of 2021))))
  (is (= "2020" (str (Year/of 2020))))
  (is (= 366 (.length (Year/of 2020))))
  (is (= "2025" (str (.plusYears (Year/of 2020) 5))))
  (is (= "2020-03" (str (.atMonth (Year/of 2020) 3))))
  (is (= "2020-02-29" (str (.atDay (Year/of 2020) 60))))
  (is (= (Year/of 2020) (Year/parse "2020")))
  (is (neg? (compare (Year/of 2019) (Year/of 2020)))))

(deftest year-month
  (is (= "2020-02" (str (YearMonth/of 2020 2))))
  (is (= 2020 (.getYear (YearMonth/of 2020 2))))
  (is (= "FEBRUARY" (str (.getMonth (YearMonth/of 2020 2)))))
  (is (= 29 (.lengthOfMonth (YearMonth/of 2020 2))))
  (is (.isLeapYear (YearMonth/of 2020 2)))
  (is (= "2021-02" (str (.plusMonths (YearMonth/of 2020 11) 3))))
  (is (= "2020-02-29" (str (.atEndOfMonth (YearMonth/of 2020 2)))))
  (is (= "2020-02-15" (str (.atDay (YearMonth/of 2020 2) 15))))
  (is (= "2021-01" (str (YearMonth/of 2020 13))))
  (is (= "2020-05" (str (YearMonth/parse "2020-05"))))
  (is (= (YearMonth/of 2020 2) (YearMonth/of 2020 2)))
  (is (neg? (compare (YearMonth/of 2020 1) (YearMonth/of 2020 2)))))
