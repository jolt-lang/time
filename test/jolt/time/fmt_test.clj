(ns jolt.time.fmt-test
  "Stage 6 gate: DateTimeFormatter. JVM-certified values. Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.fmt])
  (:import [java.time LocalDate LocalDateTime]
           [java.time.format DateTimeFormatter FormatStyle]))

(deftest patterns
  (is (= "2020-03-05" (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd") (LocalDate/of 2020 3 5))))
  (is (= "Mar 5, 2020" (.format (DateTimeFormatter/ofPattern "MMM d, yyyy") (LocalDate/of 2020 3 5))))
  (is (= "March" (.format (DateTimeFormatter/ofPattern "MMMM") (LocalDate/of 2020 3 5))))
  ;; 2020-03-05 is a Thursday
  (is (= "Thursday" (.format (DateTimeFormatter/ofPattern "EEEE") (LocalDate/of 2020 3 5))))
  (is (= "Thu" (.format (DateTimeFormatter/ofPattern "EEE") (LocalDate/of 2020 3 5))))
  (is (= "2020-03-05T13:45:30" (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss") (LocalDateTime/of 2020 3 5 13 45 30))))
  (is (= "1:45 PM" (.format (DateTimeFormatter/ofPattern "h:mm a") (LocalDateTime/of 2020 3 5 13 45))))
  (is (= "05/03/2020" (.format (DateTimeFormatter/ofPattern "dd/MM/yyyy") (LocalDate/of 2020 3 5)))))

(deftest iso-constants
  (is (= "2020-03-05" (.format DateTimeFormatter/ISO_LOCAL_DATE (LocalDate/of 2020 3 5))))
  (is (= "13:45:30" (.format DateTimeFormatter/ISO_LOCAL_TIME (LocalDateTime/of 2020 3 5 13 45 30))))
  (is (= "2020-03-05T13:45:30" (.format DateTimeFormatter/ISO_LOCAL_DATE_TIME (LocalDateTime/of 2020 3 5 13 45 30)))))

(deftest localized
  (is (= "Mar 5, 2020" (.format (DateTimeFormatter/ofLocalizedDate FormatStyle/MEDIUM) (LocalDate/of 2020 3 5))))
  (is (string? (.format (.withLocale (DateTimeFormatter/ofPattern "yyyy") (java.util.Locale. "en")) (LocalDate/of 2020 1 1)))))

(deftest localized-per-locale
  ;; Selmer's locale date assertions, as produced by the reference JVM; the date
  ;; is 2014-03-01T00:00. Note (java.util.Locale. "en_US") is a malformed
  ;; *language* subtag and resolves to ROOT (ISO-ish patterns), not to English
  ;; and not to US.
  (let [d (LocalDateTime/of 2014 3 1 0 0 0)
        fmt (fn [f id] (.format (.withLocale f (java.util.Locale. id)) d))]
    ;; month name from the bundled table; no OS locale needed
    (is (= "mars" (fmt (DateTimeFormatter/ofPattern "MMMM") "fr")))
    (is (= "00:00" (fmt (DateTimeFormatter/ofLocalizedTime FormatStyle/SHORT) "en_US")))
    (is (= "00:00" (fmt (DateTimeFormatter/ofLocalizedTime FormatStyle/SHORT) "zh")))
    (is (= "2014-03-01" (fmt (DateTimeFormatter/ofLocalizedDate FormatStyle/SHORT) "en_US")))
    (is (= "2014/3/1" (fmt (DateTimeFormatter/ofLocalizedDate FormatStyle/SHORT) "zh")))
    (is (= "2014-03-01 00:00" (fmt (DateTimeFormatter/ofLocalizedDateTime FormatStyle/SHORT) "en_US")))
    (is (= "2014/3/1 00:00" (fmt (DateTimeFormatter/ofLocalizedDateTime FormatStyle/SHORT) "zh")))
    (is (= "2014年3月1日 00:00:00" (fmt (DateTimeFormatter/ofLocalizedDateTime FormatStyle/MEDIUM) "zh")))
    (is (= "2014年3月1日" (fmt (DateTimeFormatter/ofLocalizedDate FormatStyle/LONG) "zh")))
    ;; ROOT wide month is "Mar", not "March" — CLDR ROOT abbreviates, verified on the
    ;; reference JVM. Selmer accepts either shape here and says so in its own comment.
    (is (= "2014 Mar 1" (fmt (DateTimeFormatter/ofLocalizedDate FormatStyle/LONG) "en_US")))
    ;; day name from the bundled table; 2014-03-01 is a Saturday
    (is (= "星期六" (fmt (DateTimeFormatter/ofPattern "EEEE") "zh")))))

(deftest localized-resolution
  (let [d (LocalDate/of 2014 3 1)
        short-date (fn [locale] (.format (.withLocale (DateTimeFormatter/ofLocalizedDate FormatStyle/SHORT) locale) d))]
    ;; narrowing: zh-CN has no entry of its own and falls back to zh
    (is (= "2014年3月1日"
           (.format (.withLocale (DateTimeFormatter/ofLocalizedDate FormatStyle/LONG) (java.util.Locale. "zh" "CN")) d)))
    ;; a well-formed but unknown id lands on ROOT
    (is (= "2014-03-01" (short-date (java.util.Locale. "tlh"))))
    (is (= "2014-03-01" (short-date (java.util.Locale. "xx" "YY"))))
    ;; US proper is not ROOT
    (is (= "3/1/14" (short-date (java.util.Locale. "en" "US"))))
    ;; the two-arg ctor joins language and country like the constants do
    (is (= "01.03.14" (short-date (java.util.Locale. "de" "DE"))))
    (is (= (short-date (java.util.Locale. "de" "DE")) (short-date java.util.Locale/GERMANY)))))

(deftest via-temporal-method
  (is (= "2020-03-05" (.format (LocalDate/of 2020 3 5) (DateTimeFormatter/ofPattern "yyyy-MM-dd")))))

(deftest builder-append-fraction
  ;; appendFraction maps onto the pattern model as '.' + maxWidth 'S' characters,
  ;; so output is fixed-width (the JVM trims trailing zeros toward minWidth; the
  ;; values here have none, so both agree).
  (let [bb (fn [] (doto (java.time.format.DateTimeFormatterBuilder.)
                    (.appendPattern "HH:mm:ss")))
        t (java.time.LocalTime/of 12 34 56 123456789)
        nano java.time.temporal.ChronoField/NANO_OF_SECOND]
    (is (= "12:34:56.123456789"
           (.format (.toFormatter (.appendFraction (bb) nano 0 9 true)) t)))
    (is (= "12:34:56.123"
           (.format (.toFormatter (.appendFraction (bb) nano 0 3 true)) t)))
    (is (= "12:34:56123"
           (.format (.toFormatter (.appendFraction (bb) nano 0 3 false)) t)))
    (is (= "12:34:56.123"
           (.format (.toFormatter (.appendFraction (bb) java.time.temporal.ChronoField/MILLI_OF_SECOND 0 3 true)) t)))))

(deftest builder-append-fraction-round-trip
  (let [f (.toFormatter (doto (java.time.format.DateTimeFormatterBuilder.)
                          (.appendPattern "HH:mm:ss")
                          (.appendFraction java.time.temporal.ChronoField/NANO_OF_SECOND 0 9 true)))
        s (.format f (java.time.LocalTime/of 12 34 56 123456789))]
    (is (= "12:34:56.123456789" s))
    (is (= s (.format f (.parse f s))))))

(deftest builder-append-fraction-unsupported-field
  (is (thrown? Exception
        (.appendFraction (java.time.format.DateTimeFormatterBuilder.)
                         java.time.temporal.ChronoField/YEAR 0 3 true))))
