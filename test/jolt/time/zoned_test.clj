(ns jolt.time.zoned-test
  "Stage 5b gate: ZonedDateTime, OffsetDateTime, OffsetTime, Clock, atZone/atOffset.
  JVM-certified values. Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.zoned])
  (:import [java.time ZonedDateTime OffsetDateTime LocalDateTime LocalDate Instant ZoneId ZoneOffset]
           [java.time.format DateTimeFormatter]))

(deftest zoned-date-time
  (let [ldt (LocalDateTime/of 2020 1 15 12 0 0)
        z (.atZone ldt (ZoneId/of "America/New_York"))]
    (is (= "2020-01-15T12:00-05:00[America/New_York]" (str z)))
    (is (= -18000 (.getTotalSeconds (.getOffset z))))
    (is (= "America/New_York" (.getId (.getZone z))))
    (is (= "2020-01-15T17:00:00Z" (str (.toInstant z))))
    (is (= "2020-01-15T12:00" (str (.toLocalDateTime z)))))
  ;; withZoneSameInstant keeps the instant, changes wall time
  (let [z (.atZone (LocalDateTime/of 2020 7 1 12 0) (ZoneId/of "America/New_York"))
        paris (.withZoneSameInstant z (ZoneId/of "Europe/Paris"))]
    (is (= "2020-07-01T18:00+02:00[Europe/Paris]" (str paris))))
  (is (= "2020-07-06T10:59:13.417Z"
         (str (ZonedDateTime/parse "2020-07-06T10:59:13.417Z")))))

(deftest offset-date-time
  (let [o (.atOffset (LocalDateTime/of 2020 1 15 12 0) (ZoneOffset/ofHours -5))]
    (is (= "2020-01-15T12:00-05:00" (str o)))
    (is (= "2020-01-15T17:00:00Z" (str (.toInstant o)))))
  (is (= "2020-07-06T10:59:13.417Z" (str (OffsetDateTime/parse "2020-07-06T10:59:13.417Z")))))

(deftest instant-atzone
  (let [i (Instant/parse "2020-07-06T10:59:13Z")]
    (is (= 1594033153 (.toEpochSecond (.atZone i (ZoneId/of "Europe/Paris")))))
    (is (= "2020-07-06T12:59:13+02:00[Europe/Paris]" (str (.atZone i (ZoneId/of "Europe/Paris")))))))

(deftest clock
  (let [c (java.time.Clock/fixed (Instant/ofEpochMilli 0) (ZoneId/of "UTC"))]
    (is (= 0 (.millis c)))
    (is (= "1970-01-01T00:00:00Z" (str (.instant c))))))
