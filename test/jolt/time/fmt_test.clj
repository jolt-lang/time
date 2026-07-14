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

(deftest via-temporal-method
  (is (= "2020-03-05" (.format (LocalDate/of 2020 3 5) (DateTimeFormatter/ofPattern "yyyy-MM-dd")))))
