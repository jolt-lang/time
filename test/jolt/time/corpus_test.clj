(ns jolt.time.corpus-test
  "The java.time interop rows migrated from jolt's core conformance corpus. Each
  :actual is evaluated against the library shim and compared to its JVM-certified
  :expected value."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [jolt.time])
  (:import [java.time LocalDate LocalTime LocalDateTime Instant ZonedDateTime OffsetDateTime
            OffsetTime ZoneId ZoneOffset Duration Period Year YearMonth MonthDay Clock
            DayOfWeek Month]
           [java.time.format DateTimeFormatter DateTimeFormatterBuilder FormatStyle]
           [java.time.temporal ChronoUnit ChronoField TemporalAdjusters]
           [java.util Date Locale]))

(def rows (edn/read-string (slurp "test/java_time_corpus.edn")))

;; Evaluate the case as SOURCE (like jolt's corpus runner) so #inst / #uuid reader
;; literals compile to their constructors instead of being spliced as opaque values.
(deftest java-time-corpus
  (doseq [{:keys [suite label expected actual]} rows]
    (let [want (load-string expected)
          r (try {:v (load-string actual)}
                 (catch Throwable e {:err (or (ex-message e)
                                              (when-let [cm (resolve 'jolt.host/condition-message)] ((deref cm) e))
                                              "?")}))]
      (when (or (:err r) (not= want (:v r)))
        (println "CORPUS-FAIL:" label "|" (or (:err r) (pr-str (:v r))) "| want" expected))
      (is (= want (:v r)) (str suite " / " label)))))
