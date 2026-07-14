(ns jolt.time.local-test
  "Stage 3a gate: LocalDate, LocalTime, LocalDateTime. Run under
  JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.local])
  (:import [java.time LocalDate LocalTime LocalDateTime]))

(deftest local-date
  (is (= "2020-01-15" (str (LocalDate/of 2020 1 15))))
  (is (= "WEDNESDAY" (.name (.getDayOfWeek (LocalDate/of 2020 1 15)))))
  (is (= 2020 (.getYear (LocalDate/of 2020 1 15))))
  (is (= "JANUARY" (str (.getMonth (LocalDate/of 2020 1 15)))))
  (is (= 15 (.getDayOfMonth (LocalDate/of 2020 1 15))))
  (is (= 29 (.lengthOfMonth (LocalDate/of 2020 2 1))))
  (is (.isLeapYear (LocalDate/of 2020 2 1)))
  (is (= "2020-01-04" (str (.plusDays (LocalDate/of 2020 1 1) 3))))
  (is (= "2020-03-31" (str (.plusMonths (LocalDate/of 2020 1 31) 2))))
  (is (= "2021-01-31" (str (.plusYears (LocalDate/of 2020 1 31) 1))))
  (is (= "2020-02-29" (str (.withDayOfMonth (LocalDate/of 2020 2 15) 29))))
  (is (.isBefore (LocalDate/of 2020 1 1) (LocalDate/of 2020 1 2)))
  (is (= (LocalDate/of 2020 1 15) (LocalDate/of 2020 1 15)))
  (is (= "2020-01-15" (str (LocalDate/parse "2020-01-15"))))
  (is (= 18276 (.toEpochDay (LocalDate/of 2020 1 15))))
  (is (instance? java.time.temporal.Temporal (LocalDate/of 2020 1 15)))
  (is (= [(LocalDate/of 2020 1 1) (LocalDate/of 2020 1 15)]
         (sort [(LocalDate/of 2020 1 15) (LocalDate/of 2020 1 1)]))))

(deftest local-time
  (is (= "13:45" (str (LocalTime/of 13 45))))
  (is (= "13:45:30" (str (LocalTime/of 13 45 30))))
  (is (= "00:00:00.000000123" (str (LocalTime/of 0 0 0 123))))
  (is (= 13 (.getHour (LocalTime/of 13 45 30))))
  (is (= 30 (.getSecond (LocalTime/of 13 45 30))))
  (is (= "14:45:30" (str (.plusHours (LocalTime/of 13 45 30) 1))))
  (is (= "00:45:30" (str (.plusHours (LocalTime/of 23 45 30) 1))))
  (is (= "13:45" (str (.truncatedTo (LocalTime/of 13 45 30) java.time.temporal.ChronoUnit/MINUTES))))
  (is (= (LocalTime/of 12 0) LocalTime/NOON))
  (is (= "13:45:30" (str (LocalTime/parse "13:45:30")))))

(deftest local-date-time
  (is (= "2020-01-15T13:45:30" (str (LocalDateTime/of 2020 1 15 13 45 30))))
  (is (= "2020-01-15T13:45" (str (LocalDateTime/of 2020 1 15 13 45))))
  (is (= "2020-01-15" (str (.toLocalDate (LocalDateTime/of 2020 1 15 13 45 30)))))
  (is (= "13:45:30" (str (.toLocalTime (LocalDateTime/of 2020 1 15 13 45 30)))))
  (is (= "2020-01-16T00:45:30" (str (.plusHours (LocalDateTime/of 2020 1 15 23 45 30) 1))))
  (is (= "2020-01-15T13:45:30" (str (.atTime (LocalDate/of 2020 1 15) (LocalTime/of 13 45 30)))))
  (is (= "2020-01-15T00:00" (str (.atStartOfDay (LocalDate/of 2020 1 15)))))
  (is (= "2020-03-05T13:45:30" (str (LocalDateTime/parse "2020-03-05T13:45:30"))))
  (is (.isBefore (LocalDateTime/of 2020 1 15 13 0) (LocalDateTime/of 2020 1 15 14 0)))
  (is (= (LocalDateTime/of 2020 1 15 13 45 30) (LocalDateTime/of 2020 1 15 13 45 30))))
