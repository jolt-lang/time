(ns jolt.time.util
  "Pure helpers shared across the java.time value types: calendar math, name
  tables, and the fixed unit-duration table.")

(def nanos-per-sec 1000000000)
(def nanos-per-day 86400000000000)
;; java.time's average year length (365.2425 days) for the estimated durations.
(def seconds-per-year 31556952)

(def month-names
  ["JANUARY" "FEBRUARY" "MARCH" "APRIL" "MAY" "JUNE"
   "JULY" "AUGUST" "SEPTEMBER" "OCTOBER" "NOVEMBER" "DECEMBER"])
(def day-names
  ["MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY" "FRIDAY" "SATURDAY" "SUNDAY"])

(defn leap? [y]
  (and (zero? (mod y 4)) (or (not (zero? (mod y 100))) (zero? (mod y 400)))))

(defn floor-div [a b]
  (let [q (quot a b) r (rem a b)]
    (if (and (not (zero? r)) (neg? (* a b))) (dec q) q)))

(defn floor-mod [a b] (- a (* (floor-div a b) b)))

;; java.time coerces its numeric args toward zero (truncating); a value already
;; an exact integer is left as-is so large counts keep arbitrary precision.
(defn ->long [n] (if (integer? n) n (long n)))

(defn len-of-month [y m]
  (cond (= m 2) (if (leap? y) 29 28)
        (#{4 6 9 11} m) 30
        :else 31))

;; unit -> fixed nanosecond length; FOREVER has none. month/year and up use the
;; average-year estimate, exactly as java.time's ChronoUnit does.
(def chrono-unit-nanos
  (let [s seconds-per-year n nanos-per-sec]
    {"NANOS" 1 "MICROS" 1000 "MILLIS" 1000000
     "SECONDS" n "MINUTES" (* 60 n) "HOURS" (* 3600 n) "HALF_DAYS" (* 43200 n)
     "DAYS" (* 86400 n) "WEEKS" (* 7 86400 n)
     "MONTHS" (long (Math/round (double (* (/ s 12) n))))
     "YEARS" (* s n)
     "DECADES" (* 10 s n) "CENTURIES" (* 100 s n) "MILLENNIA" (* 1000 s n)
     "ERAS" (* 1000000000 s n) "FOREVER" nil}))

;; ChronoUnit order (for ordinal / values).
(def chrono-units
  ["NANOS" "MICROS" "MILLIS" "SECONDS" "MINUTES" "HOURS" "HALF_DAYS"
   "DAYS" "WEEKS" "MONTHS" "YEARS" "DECADES" "CENTURIES" "MILLENNIA" "ERAS" "FOREVER"])

(def chrono-unit-index (zipmap chrono-units (range)))

(def chrono-fields
  ["NANO_OF_SECOND" "NANO_OF_DAY" "MICRO_OF_SECOND" "MICRO_OF_DAY" "MILLI_OF_SECOND" "MILLI_OF_DAY"
   "SECOND_OF_MINUTE" "SECOND_OF_DAY" "MINUTE_OF_HOUR" "MINUTE_OF_DAY"
   "HOUR_OF_AMPM" "CLOCK_HOUR_OF_AMPM" "HOUR_OF_DAY" "CLOCK_HOUR_OF_DAY" "AMPM_OF_DAY"
   "DAY_OF_WEEK" "ALIGNED_DAY_OF_WEEK_IN_MONTH" "ALIGNED_DAY_OF_WEEK_IN_YEAR"
   "DAY_OF_MONTH" "DAY_OF_YEAR" "EPOCH_DAY"
   "ALIGNED_WEEK_OF_MONTH" "ALIGNED_WEEK_OF_YEAR"
   "MONTH_OF_YEAR" "PROLEPTIC_MONTH" "YEAR_OF_ERA" "YEAR" "ERA"
   "INSTANT_SECONDS" "OFFSET_SECONDS"])

(def chrono-field-index (zipmap chrono-fields (range)))

(def date-based-fields
  #{"DAY_OF_WEEK" "DAY_OF_MONTH" "DAY_OF_YEAR" "EPOCH_DAY"
    "MONTH_OF_YEAR" "PROLEPTIC_MONTH" "YEAR_OF_ERA" "YEAR" "ERA"})
