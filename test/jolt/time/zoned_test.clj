(ns jolt.time.zoned-test
  "Stage 5b gate: ZonedDateTime, OffsetDateTime, OffsetTime, Clock, atZone/atOffset.
  JVM-certified values. Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.zoned])
  (:import [java.time ZonedDateTime OffsetDateTime OffsetTime LocalDateTime LocalTime LocalDate Instant ZoneId ZoneOffset Clock]
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

(defn- ex-class-name [f]
  (try (f) nil (catch Throwable e (.getName (class e)))))

;; OffsetDateTime/parse with one argument is ISO_OFFSET_DATE_TIME: the offset is
;; Z, +hh, +hh:mm or +hh:mm:ss and is required; anything else throws
;; DateTimeParseException. Expected values are the JDK's.
(deftest offset-date-time-parse-iso-shape
  (doseq [[s expected] [["2026-11-16T10:12:33.123+11:00" "2026-11-16T10:12:33.123+11:00"]
                        ["2026-11-16T10:12:33+11" "2026-11-16T10:12:33+11:00"]
                        ["2026-11-16T10:12:33+11:00:30" "2026-11-16T10:12:33+11:00:30"]
                        ["2026-11-16T10:12:33-05:30" "2026-11-16T10:12:33-05:30"]
                        ["2026-11-16T10:12:33z" "2026-11-16T10:12:33Z"]
                        ["2026-11-16t10:12:33Z" "2026-11-16T10:12:33Z"]
                        ["2026-11-16T10:12Z" "2026-11-16T10:12Z"]
                        ["2026-11-16T10:12:33.+11:00" "2026-11-16T10:12:33+11:00"]
                        ["+12026-11-16T10:12:33+11:00" "+12026-11-16T10:12:33+11:00"]
                        ["2024-02-29T23:59:59.999999999-18:00" "2024-02-29T23:59:59.999999999-18:00"]
                        ["-2026-11-16T10:12:33Z" "-2026-11-16T10:12:33Z"]
                        ["+002026-11-16T10:12:33Z" "2026-11-16T10:12:33Z"]
                        ["2026-11-16T10:12:33+17:59:59" "2026-11-16T10:12:33+17:59:59"]]]
    (is (= expected (str (OffsetDateTime/parse s))) s))
  (doseq [s ["2026-11-16T10:12:33.123+1100" "2026-11-16T10:12:33+110030" "2026-11-16T10:12:33-0530"
             "2026-11-16T10:12:33" "2026-11-16" "garbage" "" "2026-11-16T10:12:33+1:00"
             "2026-11-16T10:12:33+19:00" "2026-11-16T10:12:33+11:00[Australia/Sydney]"
             "2026-11-16T10:12:33.1234567891+11:00" "2026-11-16T10:12:33 +11:00"
             "2026-02-30T10:12:33Z" "2026-11-16T24:00:00Z" "2026-13-01T00:00Z" "2026-11-16T10:60Z"
             "2026-11-16T10:12:60Z" "2025-02-29T00:00Z"
             "2026-11-16T10:12:33+00:60" "2026-11-16T10:12:33+00:00:60" "2026-11-16T10:12:33+17:60"
             "12026-11-16T10:12:33Z" "+2026-11-16T10:12:33Z" "02026-11-16T10:12:33Z"]]
    (is (= "java.time.format.DateTimeParseException" (ex-class-name #(OffsetDateTime/parse s))) s)))

;; ZonedDateTime/parse with one argument is ISO_ZONED_DATE_TIME: an
;; ISO_OFFSET_DATE_TIME, then an optional [zone]. The offset is required, the
;; zone must exist, and the instant is read in the zone (or at the offset when
;; there is none). Expected values are the JDK's.
(deftest zoned-date-time-parse-iso-shape
  (doseq [[s expected] [["2020-01-01T00:00+01:00" "2020-01-01T00:00+01:00"]
                        ["2020-01-01T00:00Z" "2020-01-01T00:00Z"]
                        ["2020-01-01T00:00:00.5-05:30" "2020-01-01T00:00:00.500-05:30"]
                        ["2020-01-15T10:30:00+02:00[Europe/Paris]" "2020-01-15T09:30+01:00[Europe/Paris]"]
                        ["2020-07-15T10:30:00+02:00[Europe/Paris]" "2020-07-15T10:30+02:00[Europe/Paris]"]
                        ["2020-01-01T00:00Z[UTC]" "2020-01-01T00:00Z[UTC]"]
                        ["2020-01-01T00:00+01:00[UTC+01:00]" "2020-01-01T00:00+01:00[UTC+01:00]"]
                        ["2020-01-01T00:00+01:00[+01:00]" "2020-01-01T00:00+01:00"]]]
    (is (= expected (str (ZonedDateTime/parse s))) s))
  (doseq [s ["2020-01-01T00:00Z[Nope/Zone]" "2020-01-01T00:00[Europe/Paris]" "2020-01-01T00:00"
             "2020-01-01T00:00+0100" "2020-01-01" "garbage" "" "2020-01-01T00:00Z[Europe/Paris"
             "2020-01-01T00:00Z[]" "2020-01-01T00:00Z[EST]" "2020-01-01T00:00Z[Europe/Paris]x"
             "2020-02-30T00:00Z[UTC]"]]
    (is (= "java.time.format.DateTimeParseException" (ex-class-name #(ZonedDateTime/parse s))) s)))

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

;; LocalDateTime/ofInstant and ofEpochSecond read a zone or an offset, so they
;; live here rather than in core's LocalDateTime (jolt-lang/jolt#1197). Values
;; certified against the JVM.
(deftest local-date-time-of-instant
  (let [i (Instant/parse "2026-09-29T12:34:56.123456789Z")]
    (is (= "2026-09-29T12:34:56.123456789" (str (LocalDateTime/ofInstant i ZoneOffset/UTC))))
    (is (= "2026-09-29T14:34:56.123456789" (str (LocalDateTime/ofInstant i (ZoneOffset/ofHours 2)))))
    ;; a named zone, on both sides of its DST change
    (is (= "2026-09-29T08:34:56.123456789" (str (LocalDateTime/ofInstant i (ZoneId/of "America/New_York")))))
    (is (= "2026-01-15T07:00"
           (str (LocalDateTime/ofInstant (Instant/parse "2026-01-15T12:00:00Z") (ZoneId/of "America/New_York")))))
    (is (= "2026-09-29T18:04:56.123456789" (str (LocalDateTime/ofInstant i (ZoneId/of "Asia/Kolkata")))))
    ;; the same answer as the atZone chain it is defined by
    (doseq [z [ZoneOffset/UTC (ZoneOffset/ofHours -7) (ZoneId/of "Europe/Paris")]]
      (is (= (.toLocalDateTime (.atZone i z)) (LocalDateTime/ofInstant i z))))))

(deftest local-date-time-of-epoch-second
  (is (= "2026-09-29T14:34:56" (str (LocalDateTime/ofEpochSecond 1790685296 0 (ZoneOffset/ofHours 2)))))
  (is (= "2026-09-29T12:34:56" (str (LocalDateTime/ofEpochSecond 1790685296 0 ZoneOffset/UTC))))
  (is (= "1969-12-31T20:30:00.000000005"
         (str (LocalDateTime/ofEpochSecond 0 5 (ZoneOffset/ofHoursMinutes -3 -30)))))
  (is (= "1970-01-01T13:59:59.999999999"
         (str (LocalDateTime/ofEpochSecond -1 999999999 (ZoneOffset/ofHours 14)))))
  ;; the JVM's parameter is a ZoneOffset, not a ZoneId
  (is (thrown? ClassCastException (LocalDateTime/ofEpochSecond 0 0 (ZoneId/of "Europe/Paris"))))
  (is (thrown-with-msg? java.time.DateTimeException #"NanoOfSecond"
        (LocalDateTime/ofEpochSecond 0 1000000000 ZoneOffset/UTC))))

;; compareTo answers what the JDK's does, magnitude included, and casts its
;; argument: ZoneOffset orders by DESCENDING total seconds; ZonedDateTime and
;; OffsetDateTime by epoch second, then the nano difference, then the local
;; date-time (whose date part is a field difference), then the zone id.
;; JVM-certified on JDK 20.
(deftest compare-to-jdk-order
  (let [utc ZoneOffset/UTC
        cce (fn [f] (try (f) (catch ClassCastException _ :cce)))]
    (is (= [10800 -10800 ["+05:00" "+02:00" "-01:00"] -895 -1 -1 26 -3 1 -1 -1 1
            [:cce :cce :cce :cce :cce]]
           [(compare (ZoneOffset/ofHours 2) (ZoneOffset/ofHours 5))
            (.compareTo (ZoneOffset/ofHours 5) (ZoneOffset/ofHours 2))
            (mapv str (sort [(ZoneOffset/ofHours 2) (ZoneOffset/ofHours 5) (ZoneOffset/ofHours -1)]))
            (compare (ZonedDateTime/of 2020 1 1 0 0 0 5 utc) (ZonedDateTime/of 2020 1 1 0 0 0 900 utc))
            (compare (ZonedDateTime/of 2020 1 1 0 0 0 0 utc) (ZonedDateTime/of 2020 1 1 0 0 1 0 utc))
            (compare (ZonedDateTime/of 2020 1 1 12 0 0 0 utc) (ZonedDateTime/of 2020 1 1 14 0 0 0 (ZoneOffset/ofHours 2)))
            (compare (ZonedDateTime/of 2020 1 1 12 0 0 0 (ZoneId/of "Europe/Paris")) (ZonedDateTime/of 2020 1 1 12 0 0 0 (ZoneOffset/ofHours 1)))
            (compare (OffsetDateTime/of 2020 1 1 0 0 0 0 utc) (OffsetDateTime/of 2023 5 1 0 0 0 0 utc))
            (compare (OffsetDateTime/of 2020 1 1 0 0 0 5 utc) (OffsetDateTime/of 2020 1 1 0 0 0 900 (ZoneOffset/ofHours 1)))
            (compare (OffsetDateTime/of 2020 1 1 12 0 0 0 utc) (OffsetDateTime/of 2020 1 1 14 0 0 0 (ZoneOffset/ofHours 2)))
            (compare (OffsetTime/of 10 0 0 0 utc) (OffsetTime/of 12 0 0 0 (ZoneOffset/ofHours 2)))
            (compare (OffsetTime/of 10 0 0 0 utc) (OffsetTime/of 9 0 0 0 utc))
            (mapv cce [#(.compareTo (ZoneOffset/ofHours 2) 1)
                       #(.compareTo (ZonedDateTime/of 2020 1 1 0 0 0 0 utc) 1)
                       #(.compareTo (OffsetDateTime/of 2020 1 1 0 0 0 0 utc) 1)
                       #(.compareTo (OffsetTime/of 10 0 0 0 utc) 1)
                       #(.compareTo (ZonedDateTime/of 2020 1 1 0 0 0 0 utc) (OffsetDateTime/of 2020 1 1 0 0 0 0 utc))])]))))

;; OffsetTime is a TemporalAccessor: the time-of-day fields and OFFSET_SECONDS
;; answer get/getLong/isSupported, date fields and units are unsupported.
;; Values as on JDK 21.
(deftest offset-time-temporal-accessor
  (let [t (OffsetTime/of 10 30 15 0 (ZoneOffset/ofHours 2))]
    (is (= [true true true false]
           [(.isSupported t java.time.temporal.ChronoField/HOUR_OF_DAY)
            (.isSupported t java.time.temporal.ChronoField/OFFSET_SECONDS)
            (.isSupported t java.time.temporal.ChronoUnit/HOURS)
            (.isSupported t java.time.temporal.ChronoField/MONTH_OF_YEAR)]))
    (is (= false (.isSupported t java.time.temporal.ChronoUnit/DAYS)))
    (is (= 30 (.get t java.time.temporal.ChronoField/MINUTE_OF_HOUR)))
    (is (= 7200 (.get t java.time.temporal.ChronoField/OFFSET_SECONDS)))
    (is (= 37815000000000 (.getLong t java.time.temporal.ChronoField/NANO_OF_DAY)))
    (is (thrown? Exception (.get t java.time.temporal.ChronoField/MONTH_OF_YEAR)))))
