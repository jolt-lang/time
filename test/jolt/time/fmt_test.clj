(ns jolt.time.fmt-test
  "Stage 6 gate: DateTimeFormatter. JVM-certified values. Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.fmt]
            [jolt.time.impl])
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

;; java.time's ISO constants print the second's fraction only when it is
;; non-zero, with as many digits as carry a value (up to nine); values from
;; the reference.
(deftest iso-fraction
  (let [z (java.time.ZoneId/of "Australia/Sydney")
        zdt (fn [nano] (java.time.ZonedDateTime/of 2026 1 15 12 0 0 nano z))
        f DateTimeFormatter/ISO_ZONED_DATE_TIME]
    (is (= "2026-01-15T12:00:00+11:00[Australia/Sydney]" (.format f (zdt 0))))
    (is (= "2026-01-15T12:00:00.123+11:00[Australia/Sydney]" (.format f (zdt 123000000))))
    (is (= "2026-01-15T12:00:00.123456789+11:00[Australia/Sydney]" (.format f (zdt 123456789))))
    (is (= "2026-01-15T12:00:00.00012+11:00[Australia/Sydney]" (.format f (zdt 120000))))
    (is (= "2026-01-15T12:00:00.005+11:00" (.format DateTimeFormatter/ISO_OFFSET_DATE_TIME (zdt 5000000))))
    (is (= "2026-01-15T12:00:00+11:00" (.format DateTimeFormatter/ISO_OFFSET_DATE_TIME (zdt 0))))
    (is (= "12:00:00.5" (.format DateTimeFormatter/ISO_LOCAL_TIME (java.time.LocalTime/of 12 0 0 500000000))))
    (is (= "2026-01-15T12:00:00" (.format DateTimeFormatter/ISO_LOCAL_DATE_TIME (LocalDateTime/of 2026 1 15 12 0 0))))
    ;; what it prints, it parses back
    (is (= (zdt 123456789) (java.time.ZonedDateTime/parse (.format f (zdt 123456789)))))
    (is (= (zdt 0) (java.time.ZonedDateTime/parse (.format f (zdt 0))))))
  ;; ISO_INSTANT prints the fraction in groups of three digits, not trimmed
  (is (= ["1970-01-01T00:00:00Z" "1970-01-01T00:00:00.500Z" "1970-01-01T00:00:00.000120Z"
          "1970-01-01T00:00:00.123456789Z" "1970-01-01T00:00:00.005Z"]
         (mapv #(.format DateTimeFormatter/ISO_INSTANT (java.time.Instant/ofEpochSecond 0 %))
               [0 500000000 120000 123456789 5000000])))
  ;; a locale does not change what an ISO constant is
  (is (= "12:00:00.5" (.format (.withLocale DateTimeFormatter/ISO_LOCAL_TIME java.util.Locale/US)
                               (java.time.LocalTime/of 12 0 0 500000000))))
  (is (= "2026-01-15T12:00:00.5+11:00[Australia/Sydney]"
         (.format (.withLocale DateTimeFormatter/ISO_ZONED_DATE_TIME java.util.Locale/US)
                  (java.time.ZonedDateTime/of 2026 1 15 12 0 0 500000000 (java.time.ZoneId/of "Australia/Sydney"))))))

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

(deftest builder-append-fraction-micro
  ;; malli.experimental.time's formatters append MICRO_OF_SECOND
  (let [t (java.time.LocalTime/of 12 34 56 123456789)
        f (.toFormatter (doto (java.time.format.DateTimeFormatterBuilder.)
                          (.appendPattern "HH:mm:ss")
                          (.appendFraction java.time.temporal.ChronoField/MICRO_OF_SECOND 0 6 true)))]
    (is (= "12:34:56.123456" (.format f t)))))

(deftest builder-append-offset-and-defaults
  ;; the malli.transform string->date builder shape: optional fraction,
  ;; optional offsets, parse defaults
  (let [f (.toFormatter (doto (java.time.format.DateTimeFormatterBuilder.)
                          (.appendPattern "yyyy-MM-dd['T'HH:mm:ss]")
                          (.optionalStart)
                          (.appendFraction java.time.temporal.ChronoField/MICRO_OF_SECOND 0 9 true)
                          (.optionalEnd)
                          (.optionalStart)
                          (.appendOffset "+HHMMss" "Z")
                          (.optionalEnd)
                          (.parseDefaulting java.time.temporal.ChronoField/HOUR_OF_DAY 0)
                          (.parseDefaulting java.time.temporal.ChronoField/OFFSET_SECONDS 0)))]
    (is (= "2020-03-05T13:45:30" (str (.parse f "2020-03-05T13:45:30Z"))))
    (is (= "2020-03-05T00:00" (str (.parse f "2020-03-05"))))
    (is (= "2020-03-05T13:45:30.123456" (str (.parse f "2020-03-05T13:45:30.123456")))))
  (let [f (.toFormatter (doto (java.time.format.DateTimeFormatterBuilder.)
                          (.appendPattern "HH:mm:ss")
                          (.appendOffset "+HH:MM:ss" "Z")))]
    (is (= "12:34:56+01:00"
           (.format f (java.time.OffsetDateTime/of 2020 1 1 12 34 56 0
                                                   (java.time.ZoneOffset/ofHours 1)))))))

;; On the JDK (str ChronoField/NANO_OF_SECOND) is the display name
;; "NanoOfSecond"; only .name is the constant. Run the builder with the field's
;; str answering the display name, as it does there, so nothing in the builder
;; leans on str for the constant name.
(defn- display-name [nm]
  (apply str (map (fn [part] (str (subs part 0 1) (.toLowerCase (subs part 1))))
                  (.split nm "_"))))

(defn- with-jdk-field-str [f]
  (let [reg @#'jolt.time.impl/registry
        spec (get @reg :jolt.time/chrono-field)]
    (swap! reg assoc :jolt.time/chrono-field
           (assoc spec :str (fn [fld] (display-name (jolt.time.impl/field fld :name)))))
    (try (f) (finally (swap! reg assoc :jolt.time/chrono-field spec)))))

(deftest builder-reads-field-name-not-str
  (with-jdk-field-str
    (fn []
      (is (= "NanoOfSecond" (str java.time.temporal.ChronoField/NANO_OF_SECOND)))
      (let [t (java.time.LocalTime/of 12 34 56 123456789)
            build (fn [field w]
                    (.toFormatter (doto (java.time.format.DateTimeFormatterBuilder.)
                                    (.appendPattern "HH:mm:ss")
                                    (.appendFraction field 0 w true))))]
        (is (= "12:34:56.123456789"
               (.format (build java.time.temporal.ChronoField/NANO_OF_SECOND 9) t)))
        (is (= "12:34:56.123456"
               (.format (build java.time.temporal.ChronoField/MICRO_OF_SECOND 6) t)))
        (is (= "12:34:56.123"
               (.format (build java.time.temporal.ChronoField/MILLI_OF_SECOND 3) t)))))))

;; FormatStyle is a JDK enum: constants in declaration order, the Enum methods,
;; and the #object print form. Values from JDK 21. Needs a jolt that has
;; jolt.host/register-enum!; an older one only gets the constants.
(deftest format-style-enum
  (is (= "MEDIUM" (str FormatStyle/MEDIUM)))
  (when (resolve 'jolt.host/register-enum!)
  (is (= ["FULL" "LONG" "MEDIUM" "SHORT"] (mapv str (FormatStyle/values))))
  (is (= ["FULL" "LONG" "MEDIUM" "SHORT"] (mapv #(.name %) (FormatStyle/values))))
  (is (= [0 1 2 3] (mapv #(.ordinal %) (FormatStyle/values))))
  (is (= -3 (.compareTo FormatStyle/FULL FormatStyle/SHORT)))
  (is (= FormatStyle/MEDIUM (FormatStyle/valueOf "MEDIUM")))
  (is (identical? FormatStyle/MEDIUM (FormatStyle/valueOf "MEDIUM")))
  (is (= FormatStyle/LONG (Enum/valueOf FormatStyle "LONG")))
  (is (thrown? IllegalArgumentException (FormatStyle/valueOf "NOPE")))
  (is (.isEnum FormatStyle))
  (is (instance? Enum FormatStyle/SHORT))
  (is (= FormatStyle (.getDeclaringClass FormatStyle/SHORT)))
  (is (= [FormatStyle/FULL FormatStyle/SHORT] (sort [FormatStyle/SHORT FormatStyle/FULL])))
  (is (re-matches #"#object\[java\.time\.format\.FormatStyle 0x[0-9a-f]+ \"SHORT\"\]"
                  (pr-str FormatStyle/SHORT)))
  (is (= 4 (count (java.util.EnumSet/allOf FormatStyle))))
  ;; still drives the localized formatters
  (is (= "Mar 5, 2020" (.format (DateTimeFormatter/ofLocalizedDate (FormatStyle/valueOf "MEDIUM"))
                                (LocalDate/of 2020 3 5))))))

;; TextStyle is an enum, and drives Month / DayOfWeek / ChronoField
;; .getDisplayName through the names this library registers. The display names
;; need a jolt whose java.time reads them (the :field-names point); an older one
;; still gets the TextStyle constants.
(defn- field-names-point? []
  (try (jolt.host/extension-value :field-names "") true (catch Exception _ false)))
(deftest text-style-enum
  (is (= "SHORT" (str java.time.format.TextStyle/SHORT)))
  (when (resolve 'jolt.host/register-enum!)
    (is (= ["FULL" "FULL_STANDALONE" "SHORT" "SHORT_STANDALONE" "NARROW" "NARROW_STANDALONE"]
           (mapv #(.name %) (java.time.format.TextStyle/values))))
    (is (= [false true false true false true] (mapv #(.isStandalone %) (java.time.format.TextStyle/values))))
    (is (= java.time.format.TextStyle/SHORT_STANDALONE (.asStandalone java.time.format.TextStyle/SHORT)))
    (is (= java.time.format.TextStyle/NARROW (.asNormal java.time.format.TextStyle/NARROW_STANDALONE)))
    (is (identical? java.time.format.TextStyle/FULL (java.time.format.TextStyle/valueOf "FULL")))
    (is (.isEnum java.time.format.TextStyle))))
(deftest locale-prints-as-the-jdk
  (is (= "en_US" (str java.util.Locale/US)))
  (is (= "" (str java.util.Locale/ROOT)))
  (is (= "en-US" (.toLanguageTag java.util.Locale/US)))
  (is (= "und" (.toLanguageTag java.util.Locale/ROOT)))
  (is (= ["en" "US"] [(.getLanguage java.util.Locale/US) (.getCountry java.util.Locale/US)]))
  ;; locale data still resolves for a Locale printed with an underscore
  (is (= "décembre" (.format (java.text.SimpleDateFormat. "MMMM" java.util.Locale/FRENCH)
                             (java.util.Date. (* 1000 60 60 24 340))))))
(deftest display-names-by-style-and-locale
  (when (field-names-point?)
    (let [m java.time.Month/MARCH d java.time.DayOfWeek/SATURDAY
          styles [java.time.format.TextStyle/FULL java.time.format.TextStyle/SHORT java.time.format.TextStyle/NARROW
                  java.time.format.TextStyle/FULL_STANDALONE java.time.format.TextStyle/SHORT_STANDALONE]
          month (fn [l] (mapv #(.getDisplayName m % l) styles))
          dow (fn [l] (mapv #(.getDisplayName d % l) (take 3 styles)))
          field (fn [f l] (.getDisplayName f l))]
      ;; every value as JDK 21 renders it
      (is (= ["Mar" "Mar" "3" "Mar" "Mar"] (month java.util.Locale/ROOT)))
      (is (= ["March" "Mar" "M" "March" "Mar"] (month java.util.Locale/US)))
      (is (= ["mars" "mars" "M" "mars" "mars"] (month java.util.Locale/FRENCH)))
      (is (= ["März" "März" "M" "März" "Mär"] (month java.util.Locale/GERMAN)))
      (is (= ["3月" "3月" "3" "3月" "3月"] (month java.util.Locale/JAPANESE)))
      (is (= ["Saturday" "Sat" "S"] (dow java.util.Locale/US)))
      (is (= ["samedi" "sam." "S"] (dow java.util.Locale/FRENCH)))
      (is (= ["土曜日" "土" "土"] (dow java.util.Locale/JAPANESE)))
      (is (= ["zaterdag" "za" "Z"] (dow (java.util.Locale. "nl"))))
      (is (= "Day of the Week" (field java.time.temporal.ChronoField/DAY_OF_WEEK java.util.Locale/ROOT)))
      (is (= "day of the week" (field java.time.temporal.ChronoField/DAY_OF_WEEK java.util.Locale/US)))
      (is (= "jour de la semaine" (field java.time.temporal.ChronoField/DAY_OF_WEEK java.util.Locale/FRENCH)))
      (is (= "Tageshälfte" (field java.time.temporal.ChronoField/AMPM_OF_DAY java.util.Locale/GERMAN)))
      (is (= "year" (field java.time.temporal.ChronoField/YEAR java.util.Locale/US)))
      (is (= "Year" (field java.time.temporal.ChronoField/YEAR java.util.Locale/ROOT)))
      (is (= "EpochDay" (field java.time.temporal.ChronoField/EPOCH_DAY java.util.Locale/FRENCH))))))
