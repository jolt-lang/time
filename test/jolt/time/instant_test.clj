(ns jolt.time.instant-test
  "Stage 4 gate: Instant. JVM-certified values. Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.instant])
  (:import [java.time Instant LocalDateTime]
           [java.time.temporal ChronoUnit]))

(deftest instant
  (is (= 0 (.toEpochMilli (Instant/ofEpochMilli 0))))
  (is (= "1970-01-01T00:00:00Z" (str (Instant/ofEpochMilli 0))))
  (is (= 5 (.getEpochSecond (Instant/ofEpochSecond 5))))
  (is (= "1970-01-01T00:00:05Z" (str (Instant/ofEpochSecond 5))))
  (is (= "1970-01-01T00:00:15Z" (str (.plusSeconds (Instant/ofEpochSecond 5) 10))))
  (is (= "1970-01-01T00:00:05.000000123Z" (str (.plusNanos (Instant/ofEpochSecond 5) 123))))
  (is (= "1970-01-01T00:00:05Z"
         (str (.truncatedTo (.plusNanos (Instant/ofEpochSecond 5) 123) ChronoUnit/SECONDS))))
  (is (= "2020-07-06T10:59:13.417Z" (str (Instant/parse "2020-07-06T10:59:13.417Z"))))
  (is (= 1594033153417 (.toEpochMilli (Instant/parse "2020-07-06T10:59:13.417Z"))))
  (is (instance? java.time.Instant (Instant/ofEpochMilli 0)))
  (is (.isBefore (Instant/ofEpochSecond 0) (Instant/ofEpochSecond 1)))
  (is (= (Instant/ofEpochMilli 1000) (Instant/ofEpochSecond 1))))

(deftest instant-temporal
  (is (= "1970-01-02T00:00:00Z" (str (.plus (Instant/ofEpochSecond 0) 1 ChronoUnit/DAYS))))
  (is (= 60 (.between ChronoUnit/SECONDS (Instant/ofEpochSecond 0) (Instant/ofEpochSecond 60))))
  (is (= "2020-01-01T00:00:00Z" (str (.toInstant (LocalDateTime/of 2020 1 1 0 0))))))
