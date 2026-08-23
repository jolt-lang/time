(ns jolt.time.zoned-test
  "Stage 5b gate: ZonedDateTime, OffsetDateTime, OffsetTime, Clock, atZone/atOffset.
  JVM-certified values. Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.zoned])
  (:import [java.time ZonedDateTime OffsetDateTime LocalDateTime LocalTime LocalDate Instant ZoneId ZoneOffset Clock]
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

;; --- `now` answers in a zone --------------------------------------------------
;; The value types live in core, which has no zone layer, so core's `now` split
;; epoch millis into fields with no offset applied: LocalDate/now answered the
;; UTC date on every machine and ignored a zone even when handed one. Fixed
;; clocks make this machine-independent — the same instant read in three zones.

(def ^:private fixed-instant (Instant/parse "2026-08-23T21:30:00Z"))

(deftest local-now-honours-its-zone
  (let [syd (Clock/fixed fixed-instant (ZoneId/of "Australia/Sydney"))   ; +10:00
        nyc (Clock/fixed fixed-instant (ZoneId/of "America/New_York"))   ; -04:00
        utc (Clock/fixed fixed-instant (ZoneId/of "UTC"))]
    (is (= "2026-08-24" (str (LocalDate/now syd))))
    (is (= "2026-08-23" (str (LocalDate/now nyc))))
    (is (= "2026-08-23" (str (LocalDate/now utc))))
    (is (= "07:30" (str (LocalTime/now syd))))
    (is (= "17:30" (str (LocalTime/now nyc))))
    (is (= "21:30" (str (LocalTime/now utc))))
    (is (= "2026-08-24T07:30" (str (LocalDateTime/now syd))))
    (is (= "2026-08-23T17:30" (str (LocalDateTime/now nyc))))))

(deftest offset-now-carries-the-zones-offset
  ;; OffsetDateTime/now hardcoded a +00:00 offset, so it read as UTC-labelled
  ;; wall clock whatever zone it was asked about
  (is (= "2026-08-24T07:30+10:00"
         (str (OffsetDateTime/now (Clock/fixed fixed-instant (ZoneId/of "Australia/Sydney"))))))
  (is (= "2026-08-23T17:30-04:00"
         (str (OffsetDateTime/now (Clock/fixed fixed-instant (ZoneId/of "America/New_York")))))))

(deftest now-family-agrees-with-zoned
  ;; every member of the family must describe the same moment as ZonedDateTime/now,
  ;; which was the only one that already honoured a zone
  (let [clk (Clock/fixed fixed-instant (ZoneId/of "Australia/Sydney"))
        zdt (ZonedDateTime/now clk)]
    (is (= (str (.toLocalDate zdt)) (str (LocalDate/now clk))))
    (is (= (str (.toLocalTime zdt)) (str (LocalTime/now clk))))
    (is (= (str (.toLocalDateTime zdt)) (str (LocalDateTime/now clk))))))

(deftest now-accepts-a-bare-zone-argument
  ;; LocalDate.now(ZoneId) is a real JVM arity, and it used to be silently
  ;; ignored rather than rejected
  (is (some? (LocalDate/now (ZoneId/of "Australia/Sydney"))))
  (is (some? (LocalTime/now (ZoneId/of "Australia/Sydney"))))
  (is (some? (LocalDateTime/now (ZoneId/of "Australia/Sydney"))))
  (is (some? (OffsetDateTime/now (ZoneId/of "Australia/Sydney")))))
