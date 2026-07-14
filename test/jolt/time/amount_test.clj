(ns jolt.time.amount-test
  "Stage 3b gate: Duration, Period. Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.amount])
  (:import [java.time Duration Period LocalDate LocalTime]))

(deftest duration
  (is (= "PT1H" (str (Duration/ofHours 1))))
  (is (= "PT1H30M" (str (Duration/ofMinutes 90))))
  (is (= "PT5S" (str (Duration/ofSeconds 5))))
  (is (= "PT0.5S" (str (.plusNanos Duration/ZERO 500000000))))
  (is (= 90 (.getSeconds (Duration/ofSeconds 90))))
  (is (= 120 (.toMinutes (Duration/ofHours 2))))
  (is (= "PT1H30M" (str (.plusMinutes (Duration/ofHours 1) 30))))
  (is (= "PT-1H" (str (.negated (Duration/ofHours 1)))))
  (is (.isZero Duration/ZERO))
  (is (.isNegative (Duration/ofSeconds -5)))
  (is (= "PT1H30M" (str (Duration/between (LocalTime/of 10 0) (LocalTime/of 11 30)))))
  (is (= "PT1H30M" (str (Duration/parse "PT1H30M"))))
  (is (= (Duration/ofSeconds 3600) (Duration/ofHours 1)))
  (is (neg? (compare (Duration/ofHours 1) (Duration/ofHours 2)))))

(deftest period
  (is (= "P1Y2M3D" (str (Period/of 1 2 3))))
  (is (= "P5D" (str (Period/ofDays 5))))
  (is (= "P0D" (str Period/ZERO)))
  (is (= 2 (.getMonths (Period/of 1 2 3))))
  (is (= 14 (.toTotalMonths (Period/of 1 2 0))))
  (is (= "P3D" (str (.plusDays (Period/ofDays 1) 2))))
  (is (= "P1Y2M14D" (str (Period/between (LocalDate/of 2020 1 1) (LocalDate/of 2021 3 15)))))
  (is (= "P1Y2M3D" (str (Period/parse "P1Y2M3D"))))
  (is (= (Period/of 1 2 3) (Period/of 1 2 3)))
  (is (instance? java.time.temporal.TemporalAmount (Period/ofDays 1))))
