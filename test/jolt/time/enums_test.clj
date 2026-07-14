(ns jolt.time.enums-test
  "Stage 2 gate: Month, DayOfWeek, ChronoUnit, ChronoField, ValueRange. Expected
  values are the JVM-certified ones from jolt's corpus. Run under
  JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.enums]))

(deftest month
  (is (= "FEBRUARY" (.toString (java.time.Month/of 2))))
  (is (= "FEBRUARY" (str (java.time.Month/of 2))))
  (is (= 29 (.length (java.time.Month/of 2) true)))
  (is (= 28 (.length (java.time.Month/of 2) false)))
  (is (= 3 (.getValue java.time.Month/MARCH)))
  (is (= 2 (.ordinal java.time.Month/MARCH)))
  (is (= "JANUARY" (str (.plus java.time.Month/DECEMBER 1))))
  (is (= "NOVEMBER" (str (.minus java.time.Month/JANUARY 2))))
  (is (= java.time.Month/JUNE (java.time.Month/valueOf "JUNE")))
  (is (= 12 (count (java.time.Month/values))))
  (is (instance? java.time.Month java.time.Month/JANUARY))
  (is (instance? java.time.temporal.TemporalAccessor java.time.Month/JANUARY))
  (is (neg? (compare java.time.Month/JANUARY java.time.Month/MARCH)))
  (is (= [java.time.Month/JANUARY java.time.Month/MARCH]
         (sort [java.time.Month/MARCH java.time.Month/JANUARY]))))

(deftest day-of-week
  (is (= "MONDAY" (str java.time.DayOfWeek/MONDAY)))
  (is (= "WEDNESDAY" (.name (java.time.DayOfWeek/of 3))))
  (is (= 7 (.getValue java.time.DayOfWeek/SUNDAY)))
  (is (= "MONDAY" (str (.plus java.time.DayOfWeek/SUNDAY 1))))
  (is (= 7 (count (java.time.DayOfWeek/values))))
  (is (instance? java.time.DayOfWeek java.time.DayOfWeek/FRIDAY)))

(deftest chrono-unit
  (is (= "DAYS" (.name java.time.temporal.ChronoUnit/DAYS)))
  (is (= "DAYS" (str java.time.temporal.ChronoUnit/DAYS)))
  (is (.isDateBased java.time.temporal.ChronoUnit/DAYS))
  (is (.isTimeBased java.time.temporal.ChronoUnit/HOURS))
  (is (instance? java.time.temporal.ChronoUnit java.time.temporal.ChronoUnit/SECONDS))
  (is (= java.time.temporal.ChronoUnit/MONTHS (java.time.temporal.ChronoUnit/valueOf "MONTHS"))))

(deftest chrono-field
  (is (= "YEAR" (.name java.time.temporal.ChronoField/YEAR)))
  (is (.isDateBased java.time.temporal.ChronoField/DAY_OF_MONTH))
  (is (.isTimeBased java.time.temporal.ChronoField/HOUR_OF_DAY))
  (is (instance? java.time.temporal.ChronoField java.time.temporal.ChronoField/YEAR)))

(deftest value-range
  (let [r (java.time.temporal.ValueRange/of 1 12)]
    (is (= 1 (.getMinimum r)))
    (is (= 12 (.getMaximum r)))
    (is (.isValidValue r 6))
    (is (not (.isValidValue r 13)))))
