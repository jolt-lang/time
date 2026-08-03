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

;; --- DateTimeFormatterBuilder -------------------------------------------------
;; Values certified against reference java.time (JDK 20). The builder composes a
;; pattern string, so what these pin is that each append* contributes the piece
;; that means the same thing, and that optional sections are attempted and then
;; skipped rather than mis-aligning the parse.

(deftest builder-composes-and-formats
  (is (= "2020-03-05 ok"
         (.format (-> (java.time.format.DateTimeFormatterBuilder.)
                      (.appendPattern "yyyy-MM-dd")
                      (.appendLiteral " ok")
                      (.toFormatter))
                  (LocalDate/of 2020 3 5)))))

(defn- ^:private malli-style-formatter
  "The shape malli.transform builds for its :string inst decoder."
  []
  (-> (java.time.format.DateTimeFormatterBuilder.)
      (.appendPattern "yyyy-MM-dd['T'HH:mm:ss]")
      (.optionalStart)
      (.appendFraction java.time.temporal.ChronoField/MICRO_OF_SECOND 0 9 true)
      (.optionalEnd)
      (.optionalStart)
      (.appendOffset "+HHMMss" "Z")
      (.optionalEnd)
      (.parseDefaulting java.time.temporal.ChronoField/HOUR_OF_DAY 0)
      (.parseDefaulting java.time.temporal.ChronoField/OFFSET_SECONDS 0)
      (.toFormatter)))

(deftest builder-optional-sections-parse
  (let [f (malli-style-formatter)
        inst-of (fn [s] (str (java.time.Instant/from (.parse f s))))]
    ;; every optional section present
    (is (= "2020-03-05T13:45:30Z" (inst-of "2020-03-05T13:45:30Z")))
    ;; the fraction section applies
    (is (= "2020-03-05T13:45:30.123456Z" (inst-of "2020-03-05T13:45:30.123456Z")))
    ;; the offset is APPLIED, not dropped: +0200 is two hours earlier in UTC
    (is (= "2020-03-05T11:45:30Z" (inst-of "2020-03-05T13:45:30+0200")))
    ;; date alone — every optional section is skipped, and the walk stays aligned
    (is (= "2020-03-05T00:00:00Z" (inst-of "2020-03-05")))))

(deftest builder-fraction-scales-by-digits-present
  ;; a 9-wide fraction field given 3 digits is milliseconds, not nanoseconds
  (let [f (-> (java.time.format.DateTimeFormatterBuilder.)
              (.appendPattern "yyyy-MM-dd'T'HH:mm:ss")
              (.optionalStart)
              (.appendFraction java.time.temporal.ChronoField/NANO_OF_SECOND 0 9 true)
              (.optionalEnd)
              (.toFormatter))]
    (is (= "2020-03-05T13:45:30.417"
           (str (.parse f "2020-03-05T13:45:30.417"))))))
