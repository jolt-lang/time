(ns jolt.time.temporal-test
  "Stage 3d gate: the generic temporal machinery. Expected values are JVM-certified
  (jolt corpus chrono-fields + java.time interop rows). Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.temporal])
  (:import [java.time LocalDate LocalTime LocalDateTime Year YearMonth Duration Period DayOfWeek]
           [java.time.temporal ChronoField ChronoUnit TemporalAdjusters]))

(deftest chrono-fields
  (is (= 24 (.getLong (LocalTime/of 0 30) ChronoField/CLOCK_HOUR_OF_DAY)))
  (is (= 2 (.getLong (LocalTime/of 14 0) ChronoField/HOUR_OF_AMPM)))
  (is (= 12 (.getLong (LocalTime/of 12 0) ChronoField/CLOCK_HOUR_OF_AMPM)))
  (is (= 3 (.getLong (LocalDate/of 2020 3 17) ChronoField/ALIGNED_DAY_OF_WEEK_IN_MONTH)))
  (is (= 3 (.getLong (LocalDate/of 2020 3 17) ChronoField/ALIGNED_WEEK_OF_MONTH)))
  (is (= 2020 (.getLong (Year/of 2020) ChronoField/YEAR)))
  (is (= 24244 (.getLong (YearMonth/of 2020 5) ChronoField/PROLEPTIC_MONTH)))
  (is (= 15 (.get (LocalDate/of 2020 1 15) ChronoField/DAY_OF_MONTH)))
  (is (.isSupported (LocalDate/of 2020 1 1) ChronoField/YEAR))
  (is (not (.isSupported (LocalDate/of 2020 1 1) ChronoField/HOUR_OF_DAY))))

(deftest plus-until-by-unit
  (is (= 14 (.between ChronoUnit/DAYS (LocalDate/of 2020 1 1) (LocalDate/of 2020 1 15))))
  (is (= "2020-01-04" (str (.plus (LocalDate/of 2020 1 1) 3 ChronoUnit/DAYS))))
  (is (= "2020-03-01" (str (.plus (LocalDate/of 2020 1 1) 2 ChronoUnit/MONTHS))))
  (is (= 31 (.until (LocalDate/of 2020 1 1) (LocalDate/of 2020 2 1) ChronoUnit/DAYS)))
  (is (= "2019-12-31" (str (.minus (LocalDate/of 2020 1 1) 1 ChronoUnit/DAYS)))))

(deftest with-field
  (is (= "2021-01-15" (str (.with (LocalDate/of 2020 1 15) ChronoField/YEAR 2021))))
  (is (= "2020-06-15" (str (.with (LocalDate/of 2020 1 15) ChronoField/MONTH_OF_YEAR 6)))))

(deftest amounts-applied
  (is (= "2020-01-01T12:00" (str (.plus (LocalDateTime/of 2020 1 1 10 0) (Duration/ofHours 2)))))
  (is (= "2020-03-01" (str (.plus (LocalDate/of 2020 1 1) (Period/ofMonths 2)))))
  (is (= "2020-01-04" (str (.addTo (Duration/ofDays 3) (LocalDate/of 2020 1 1)))))
  (is (= "2021-01-01" (str (.addTo (Period/ofYears 1) (LocalDate/of 2020 1 1))))))

(deftest adjusters
  (is (= "2020-01-01" (str (.with (LocalDate/of 2020 1 15) (TemporalAdjusters/firstDayOfMonth)))))
  (is (= "2020-01-31" (str (.with (LocalDate/of 2020 1 15) (TemporalAdjusters/lastDayOfMonth)))))
  (is (= "2020-02-01" (str (.with (LocalDate/of 2020 1 15) (TemporalAdjusters/firstDayOfNextMonth)))))
  ;; 2020-01-15 is a Wednesday; next Monday is 2020-01-20
  (is (= "2020-01-20" (str (.with (LocalDate/of 2020 1 15) (TemporalAdjusters/next DayOfWeek/MONDAY)))))
  (is (= "2020-01-13" (str (.with (LocalDate/of 2020 1 15) (TemporalAdjusters/previous DayOfWeek/MONDAY))))))
