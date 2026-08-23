(ns jolt.time.zoned
  "ZonedDateTime, OffsetDateTime, OffsetTime, and Clock, plus the deferred
  atZone/atOffset on LocalDateTime and Instant. Each zoned value keeps an
  underlying LocalDateTime/LocalTime and delegates its date/time arithmetic to it,
  re-wrapping with the same offset/zone."
  (:require [jolt.time.impl :as impl :refer [civil-from-days days-from-civil]]
            [jolt.time.util :as u]
            [jolt.time.enums :as e]
            [jolt.time.local :as l]
            [jolt.time.amount :as a]
            [jolt.time.instant :as inst]
            [jolt.time.temporal :as t]
            [jolt.time.zones :as z]))

(defn- statics! [names members] (doseq [n names] (__register-class-statics! n members)))
(def ^:private nps u/nanos-per-sec)
(def ^:private npd u/nanos-per-day)

(defn- offset-suffix [secs] (z/zo-id secs))

;; the ZonedDateTime methods below call the OffsetDateTime constructor
;; (toOffsetDateTime), which the OffsetDateTime section defines further down.
;; Declare it so the reference resolves when the method map is compiled.
(declare odt)

;; --- ZonedDateTime -----------------------------------------------------------
(defn zdt [ed nod off zid] (impl/value :jolt.time/zoned-date-time {:ed ed :nod nod :off off :zid zid}))
(defn- zdt-ed [x] (impl/field x :ed))
(defn- zdt-nod [x] (impl/field x :nod))
(defn- zdt-off [x] (impl/field x :off))
(defn- zdt-zid [x] (impl/field x :zid))
(defn zdt? [x] (= :jolt.time/zoned-date-time (impl/type-of x)))
(defn- zdt-ldt [x] (l/local-dt (zdt-ed x) (zdt-nod x)))
(defn- zdt->nanos [x] (- (+ (* (zdt-ed x) npd) (zdt-nod x)) (* (zdt-off x) nps)))
(defn- zdt->ms [x] (u/floor-div (zdt->nanos x) 1000000))
(defn- rewrap [x ldt] (zdt (l/ldt-epoch-day ldt) (l/ldt-nod ldt) (zdt-off x) (zdt-zid x)))

(defn zoned-of-ldt [ldt zone]
  (let [[id off0] (z/resolve-zone zone)
        lsecs (+ (* (l/ldt-epoch-day ldt) 86400) (u/floor-div (l/ldt-nod ldt) nps))
        off (z/zone-offset-at-local id off0 lsecs)]
    (zdt (l/ldt-epoch-day ldt) (l/ldt-nod ldt) off (z/zone-id id off))))
(defn zoned-of-instant [nanos zone]
  (let [[id off0] (z/resolve-zone zone)
        off (z/zone-offset-at-instant id off0 (u/floor-div nanos nps))
        local (+ nanos (* off nps))]
    (zdt (u/floor-div local npd) (u/floor-mod local npd) off (z/zone-id id off))))

(defn- zdt->string [x]
  (let [id (z/zid-id (zdt-zid x)) suf (offset-suffix (zdt-off x))]
    (str (l/iso-datetime-str (zdt-ed x) (zdt-nod x)) suf
         (if (or (= id suf) (and (pos? (count id)) (#{\+ \- \Z} (nth id 0)))) "" (str "[" id "]")))))

(defn parse-zoned [s]
  ;; "<local>[±HH:mm | Z][zone]" — a [zone] with no explicit offset means the
  ;; local part is wall time in that zone; otherwise it's an instant + zone.
  (let [zb (.indexOf s "[")
        zone (when (>= zb 0) (subs s (inc zb) (dec (count s))))
        body (if (>= zb 0) (subs s 0 zb) s)
        ti (.indexOf body "T")
        has-off (some (fn [i] (#{\Z \z \+ \-} (nth body i))) (range (inc ti) (count body)))]
    (if (and zone (not has-off))
      (zoned-of-ldt (l/local-dt (l/parse-iso-date (subs body 0 ti)) (u/parse-hms->nano (subs body (inc ti)))) zone)
      (zoned-of-instant (inst/parse-iso-instant body) (or zone "Z")))))

(impl/register-type! :jolt.time/zoned-date-time
  {:eq (fn [a b] (= [(zdt-ed a) (zdt-nod a) (zdt-off a) (z/zid-id (zdt-zid a))]
                    [(zdt-ed b) (zdt-nod b) (zdt-off b) (z/zid-id (zdt-zid b))]))
   :hash (fn [x] (hash (zdt->nanos x))) :str zdt->string
   :cmp (fn [a b] (compare (zdt->nanos a) (zdt->nanos b)))
   :classes #{"java.time.ZonedDateTime" "ZonedDateTime"
              "java.time.chrono.ChronoZonedDateTime" "ChronoZonedDateTime"
              "java.time.temporal.Temporal" "Temporal" "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.lang.Comparable" "Comparable"}})

(defn- other-nanos [o]
  (condp = (impl/type-of o)
    :jolt.time/zoned-date-time (zdt->nanos o)
    :jolt.time/offset-date-time (- (+ (* (impl/field o :ed) npd) (impl/field o :nod)) (* (impl/field o :off) nps))
    :jolt.time/instant (inst/inst-nanos o)
    (throw (ex-info "not a comparable instant" {:o o}))))

(__register-class-methods! :jolt.time/zoned-date-time
  (merge
   {"getYear" (fn [x] (first (civil-from-days (zdt-ed x)))) "getMonthValue" (fn [x] (second (civil-from-days (zdt-ed x))))
    "getMonth" (fn [x] (e/month (second (civil-from-days (zdt-ed x))))) "getDayOfMonth" (fn [x] (nth (civil-from-days (zdt-ed x)) 2))
    "getDayOfWeek" (fn [x] (e/dow (inc (u/floor-mod (+ (zdt-ed x) 3) 7))))
    "getHour" (fn [x] (t/get-field (zdt-ldt x) "HOUR_OF_DAY")) "getMinute" (fn [x] (t/get-field (zdt-ldt x) "MINUTE_OF_HOUR"))
    "getSecond" (fn [x] (t/get-field (zdt-ldt x) "SECOND_OF_MINUTE")) "getNano" (fn [x] (t/get-field (zdt-ldt x) "NANO_OF_SECOND"))
    "getOffset" (fn [x] (z/zone-offset (zdt-off x))) "getZone" (fn [x] (zdt-zid x))
    "toInstant" (fn [x & _] (inst/instant (zdt->nanos x)))
    "toLocalDate" (fn [x] (l/local-date (zdt-ed x))) "toLocalTime" (fn [x] (l/local-time (zdt-nod x)))
    "toLocalDateTime" (fn [x] (zdt-ldt x))
    "toOffsetDateTime" (fn [x] (odt (zdt-ed x) (zdt-nod x) (zdt-off x)))
    "toEpochSecond" (fn [x] (u/floor-div (zdt->ms x) 1000))
    "plusDays" (fn [x n] (rewrap x (l/local-dt (+ (zdt-ed x) (u/->long n)) (zdt-nod x))))
    "minusDays" (fn [x n] (rewrap x (l/local-dt (- (zdt-ed x) (u/->long n)) (zdt-nod x))))
    "plusWeeks" (fn [x n] (rewrap x (l/local-dt (+ (zdt-ed x) (* 7 (u/->long n))) (zdt-nod x))))
    "plusMonths" (fn [x n] (rewrap x (t/plus-unit (zdt-ldt x) (u/->long n) "MONTHS")))
    "minusMonths" (fn [x n] (rewrap x (t/plus-unit (zdt-ldt x) (- (u/->long n)) "MONTHS")))
    "plusYears" (fn [x n] (rewrap x (t/plus-unit (zdt-ldt x) (u/->long n) "YEARS")))
    "minusYears" (fn [x n] (rewrap x (t/plus-unit (zdt-ldt x) (- (u/->long n)) "YEARS")))
    "plusHours" (fn [x n] (rewrap x (l/ldt-plus-nanos (zdt-ldt x) (* (u/->long n) 3600 nps))))
    "minusHours" (fn [x n] (rewrap x (l/ldt-plus-nanos (zdt-ldt x) (- (* (u/->long n) 3600 nps)))))
    "plusMinutes" (fn [x n] (rewrap x (l/ldt-plus-nanos (zdt-ldt x) (* (u/->long n) 60 nps))))
    "plusSeconds" (fn [x n] (rewrap x (l/ldt-plus-nanos (zdt-ldt x) (* (u/->long n) nps))))
    "plusNanos" (fn [x n] (rewrap x (l/ldt-plus-nanos (zdt-ldt x) (u/->long n))))
    "minusSeconds" (fn [x n] (rewrap x (l/ldt-plus-nanos (zdt-ldt x) (- (* (u/->long n) nps)))))
    "minusMinutes" (fn [x n] (rewrap x (l/ldt-plus-nanos (zdt-ldt x) (- (* (u/->long n) 60 nps)))))
    "minusNanos" (fn [x n] (rewrap x (l/ldt-plus-nanos (zdt-ldt x) (- (u/->long n)))))
    "withYear" (fn [x v] (rewrap x (t/with-field (zdt-ldt x) "YEAR" (u/->long v))))
    "withMonth" (fn [x v] (rewrap x (t/with-field (zdt-ldt x) "MONTH_OF_YEAR" (u/->long v))))
    "withDayOfMonth" (fn [x v] (rewrap x (t/with-field (zdt-ldt x) "DAY_OF_MONTH" (u/->long v))))
    "withHour" (fn [x v] (rewrap x (t/with-field (zdt-ldt x) "HOUR_OF_DAY" (u/->long v))))
    "withMinute" (fn [x v] (rewrap x (t/with-field (zdt-ldt x) "MINUTE_OF_HOUR" (u/->long v))))
    "withSecond" (fn [x v] (rewrap x (t/with-field (zdt-ldt x) "SECOND_OF_MINUTE" (u/->long v))))
    "withNano" (fn [x v] (rewrap x (t/with-field (zdt-ldt x) "NANO_OF_SECOND" (u/->long v))))
    "truncatedTo" (fn [x unit] (rewrap x (l/local-dt (zdt-ed x) (let [d (or (u/chrono-unit-nanos (u/upper (e/cu-name unit))) 1)] (* (quot (zdt-nod x) d) d)))))
    "withZoneSameInstant" (fn [x zone] (zoned-of-instant (zdt->nanos x) zone))
    "withZoneSameLocal" (fn [x zone] (zoned-of-ldt (zdt-ldt x) zone))
    "plus" (fn ([x amt] (rewrap x (if (a/dur? amt) (l/ldt-plus-nanos (zdt-ldt x) (a/dur-total-nanos amt)) (t/plus-period (zdt-ldt x) amt 1))))
               ([x n unit] (rewrap x (t/plus-unit (zdt-ldt x) (u/->long n) (u/upper (e/cu-name unit))))))
    "minus" (fn ([x amt] (rewrap x (if (a/dur? amt) (l/ldt-plus-nanos (zdt-ldt x) (- (a/dur-total-nanos amt))) (t/plus-period (zdt-ldt x) amt -1))))
                ([x n unit] (rewrap x (t/plus-unit (zdt-ldt x) (- (u/->long n)) (u/upper (e/cu-name unit))))))
    "until" (fn [x o unit] (t/unit-between (u/upper (e/cu-name unit)) (zdt-ldt x) (zdt-ldt o)))
    "get" (fn [x f] (t/get-field (zdt-ldt x) (e/cf-name f))) "getLong" (fn [x f] (t/get-field (zdt-ldt x) (e/cf-name f)))
    "isBefore" (fn [x o] (< (zdt->nanos x) (other-nanos o))) "isAfter" (fn [x o] (> (zdt->nanos x) (other-nanos o)))
    "isEqual" (fn [x o] (= (zdt->nanos x) (other-nanos o)))
    "compareTo" (fn [x o] (compare (zdt->nanos x) (zdt->nanos o)))
    "equals" (fn [x o] (boolean (and (impl/jt? o) (zdt? o) (= (zdt->nanos x) (zdt->nanos o)) (= (z/zid-id (zdt-zid x)) (z/zid-id (zdt-zid o))))))
    "hashCode" (fn [x] (hash (zdt->nanos x))) "toString" zdt->string}))

;; --- what zone a `now` answers in ---------------------------------------------
;; The java.time value types live in core, which has no zone layer, so core's
;; `now` reads epoch millis and splits them into fields with no offset applied.
;; That is right for core alone and wrong once this library is loaded: it made
;; LocalDate/now answer the UTC date on every machine, and ignore a zone even
;; when handed one — (LocalDate/now (ZoneId/of "Australia/Sydney")) came back a
;; day behind. ZonedDateTime/now was the only member of the family that honoured
;; a zone. These re-register the rest over core's, the way fmt.clj already
;; re-registers the formatter-aware LocalDate/parse over core's ISO-only one.
(defn- now-zone
  "The zone a `now` answers in: a Clock's own zone, a zone designator passed
  directly, or the machine's."
  [arg]
  (cond
    (and (impl/jt? arg) (= :jolt.time/clock (impl/type-of arg))) (impl/field arg :zone)
    (some? arg) arg
    :else (z/system-zone-id)))

(defn- now-local-nanos
  "[local-nanos offset-seconds] for a Clock / zone designator / nil. clock-millis
  honours a fixed, offset or tick Clock and falls back to the wall clock for
  anything else, so a bare ZoneId argument reads the wall clock in that zone."
  [arg]
  (let [nanos (* (impl/clock-millis arg) 1000000)
        [id off0] (z/resolve-zone (now-zone arg))
        off (z/zone-offset-at-instant id off0 (u/floor-div nanos nps))]
    [(+ nanos (* off nps)) off]))

(statics! ["ZonedDateTime" "java.time.ZonedDateTime"]
  {"of" (fn ([ldt zone] (zoned-of-ldt ldt zone))
            ([d tm zone] (zoned-of-ldt (l/local-dt (l/ld-epoch-day d) (l/lt-nano-of-day tm)) zone))
            ([y mo d h mi s nano zone] (zoned-of-ldt (l/local-dt (days-from-civil (u/->long y) (u/->long mo) (u/->long d))
                                                                 (u/hmsn->nano (u/->long h) (u/->long mi) (u/->long s) (u/->long nano))) zone)))
   "ofInstant" (fn ([i zone] (zoned-of-instant (inst/inst-nanos i) zone))
                   ([ldt _off zone] (zoned-of-ldt ldt zone)))
   "now" (fn [& args] (let [clk (first args)]
                       (zoned-of-instant (* (impl/clock-millis clk) 1000000) (now-zone clk))))
   "parse" (fn [s & _] (parse-zoned (str s)))})

;; --- OffsetDateTime ----------------------------------------------------------
(defn odt [ed nod off] (impl/value :jolt.time/offset-date-time {:ed ed :nod nod :off off}))
(defn- odt-ed [x] (impl/field x :ed)) (defn- odt-nod [x] (impl/field x :nod)) (defn- odt-off [x] (impl/field x :off))
(defn odt? [x] (= :jolt.time/offset-date-time (impl/type-of x)))
(defn- odt-ldt [x] (l/local-dt (odt-ed x) (odt-nod x)))
(defn- odt->nanos [x] (- (+ (* (odt-ed x) npd) (odt-nod x)) (* (odt-off x) nps)))
(defn- orewrap [x ldt] (odt (l/ldt-epoch-day ldt) (l/ldt-nod ldt) (odt-off x)))
(defn- offset-of-ldt [ldt off] (odt (l/ldt-epoch-day ldt) (l/ldt-nod ldt) (z/parse-zone-offset (if (impl/jt? off) (z/zo-id (z/zo-secs off)) off))))
(defn- odt->string [x] (str (l/iso-datetime-str (odt-ed x) (odt-nod x)) (offset-suffix (odt-off x))))

(impl/register-type! :jolt.time/offset-date-time
  {:eq (fn [a b] (= (odt->nanos a) (odt->nanos b))) :hash (fn [x] (hash (odt->nanos x))) :str odt->string
   :cmp (fn [a b] (compare (odt->nanos a) (odt->nanos b)))
   :classes #{"java.time.OffsetDateTime" "OffsetDateTime" "java.time.temporal.Temporal" "Temporal"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor" "java.lang.Comparable" "Comparable"}})

(__register-class-methods! :jolt.time/offset-date-time
  {"getYear" (fn [x] (first (civil-from-days (odt-ed x)))) "getMonthValue" (fn [x] (second (civil-from-days (odt-ed x))))
   "getDayOfMonth" (fn [x] (nth (civil-from-days (odt-ed x)) 2)) "getHour" (fn [x] (t/get-field (odt-ldt x) "HOUR_OF_DAY"))
   "getOffset" (fn [x] (z/zone-offset (odt-off x)))
   "toInstant" (fn [x & _] (inst/instant (odt->nanos x)))
   "toLocalDate" (fn [x] (l/local-date (odt-ed x))) "toLocalTime" (fn [x] (l/local-time (odt-nod x)))
   "toLocalDateTime" (fn [x] (odt-ldt x))
   "toZonedDateTime" (fn [x] (zdt (odt-ed x) (odt-nod x) (odt-off x) (z/zone-id (z/zo-id (odt-off x)) (odt-off x))))
   "toEpochSecond" (fn [x] (u/floor-div (odt->nanos x) nps))
   "plusDays" (fn [x n] (orewrap x (l/local-dt (+ (odt-ed x) (u/->long n)) (odt-nod x))))
   "plusHours" (fn [x n] (orewrap x (l/ldt-plus-nanos (odt-ldt x) (* (u/->long n) 3600 nps))))
   "plusSeconds" (fn [x n] (orewrap x (l/ldt-plus-nanos (odt-ldt x) (* (u/->long n) nps))))
   "withOffsetSameInstant" (fn [x off] (let [secs (z/zo-secs off) local (+ (odt->nanos x) (* secs nps))]
                                         (odt (u/floor-div local npd) (u/floor-mod local npd) secs)))
   "withOffsetSameLocal" (fn [x off] (odt (odt-ed x) (odt-nod x) (z/zo-secs off)))
   "atZoneSameInstant" (fn [x zone] (zoned-of-instant (odt->nanos x) zone))
   "get" (fn [x f] (t/get-field (odt-ldt x) (e/cf-name f))) "getLong" (fn [x f] (t/get-field (odt-ldt x) (e/cf-name f)))
   "isBefore" (fn [x o] (< (odt->nanos x) (other-nanos o))) "isAfter" (fn [x o] (> (odt->nanos x) (other-nanos o)))
   "isEqual" (fn [x o] (= (odt->nanos x) (other-nanos o)))
   "compareTo" (fn [x o] (compare (odt->nanos x) (odt->nanos o)))
   "equals" (fn [x o] (boolean (and (impl/jt? o) (odt? o) (= (odt->nanos x) (odt->nanos o)))))
   "hashCode" (fn [x] (hash (odt->nanos x))) "toString" odt->string})

(statics! ["OffsetDateTime" "java.time.OffsetDateTime"]
  {"of" (fn ([ldt off] (offset-of-ldt ldt off))
            ([d tm off] (offset-of-ldt (l/local-dt (l/ld-epoch-day d) (l/lt-nano-of-day tm)) off))
            ([y mo d h mi s nano off] (offset-of-ldt (l/local-dt (days-from-civil (u/->long y) (u/->long mo) (u/->long d)) (u/hmsn->nano (u/->long h) (u/->long mi) (u/->long s) (u/->long nano))) off)))
   "now" (fn [& args] (let [[local off] (now-local-nanos (first args))]
                        (odt (u/floor-div local npd) (u/floor-mod local npd) off)))
   "ofInstant" (fn [i zone] (let [nanos (inst/inst-nanos i) [id off0] (z/resolve-zone zone)
                                  off (z/zone-offset-at-instant id off0 (u/floor-div nanos nps)) local (+ nanos (* off nps))]
                              (odt (u/floor-div local npd) (u/floor-mod local npd) off)))
   "MIN" (odt (days-from-civil -999999999 1 1) 0 (* 18 3600))
   "MAX" (odt (days-from-civil 999999999 12 31) (dec npd) (* -18 3600))
   "parse" (fn [s & _] (let [nanos (inst/parse-iso-instant (str s))
                             op (some (fn [i] (when (#{\Z \z \+ \-} (nth (str s) i)) i)) (range (inc (.indexOf (str s) "T")) (count (str s))))
                             off (z/parse-zone-offset (if op (subs (str s) op) "Z"))
                             local (+ nanos (* off nps))]
                         (odt (u/floor-div local npd) (u/floor-mod local npd) off)))})

;; --- OffsetTime --------------------------------------------------------------
(defn ot [nod off] (impl/value :jolt.time/offset-time {:nod nod :off off}))
(defn- ot-nod [x] (impl/field x :nod)) (defn- ot-off [x] (impl/field x :off))
(defn- zo-secs* [off] (cond (and (impl/jt? off) (z/zo? off)) (z/zo-secs off)
                            (and (impl/jt? off) (z/zid? off)) (z/zid-off off) :else (z/parse-zone-offset off)))
(impl/register-type! :jolt.time/offset-time
  {:eq (fn [a b] (and (= (ot-nod a) (ot-nod b)) (= (ot-off a) (ot-off b)))) :hash (fn [x] (ot-nod x))
   :str (fn [x] (str (u/iso-time-str (ot-nod x)) (offset-suffix (ot-off x))))
   ;; OffsetTime.compareTo: the UTC-equivalent instant first, local time as the
   ;; tie-break.
   :cmp (fn [a b] (let [c (compare (- (ot-nod a) (* (ot-off a) nps)) (- (ot-nod b) (* (ot-off b) nps)))]
                    (if (zero? c) (compare (ot-nod a) (ot-nod b)) c)))
   :classes #{"java.time.OffsetTime" "OffsetTime" "java.time.temporal.Temporal" "Temporal" "java.lang.Comparable" "Comparable"}})
(__register-class-methods! :jolt.time/offset-time
  {"getHour" (fn [x] (l/lt-hour (ot-nod x))) "getMinute" (fn [x] (l/lt-minute (ot-nod x)))
   "getSecond" (fn [x] (l/lt-second (ot-nod x))) "getNano" (fn [x] (l/lt-nano (ot-nod x)))
   "getOffset" (fn [x] (z/zone-offset (ot-off x))) "toLocalTime" (fn [x] (l/local-time (ot-nod x)))
   "compareTo" (fn [x o] (let [c (compare (- (ot-nod x) (* (ot-off x) nps)) (- (ot-nod o) (* (ot-off o) nps)))]
                           (if (zero? c) (compare (ot-nod x) (ot-nod o)) c)))
   "isBefore" (fn [x o] (< (- (ot-nod x) (* (ot-off x) nps)) (- (ot-nod o) (* (ot-off o) nps))))
   "isAfter" (fn [x o] (> (- (ot-nod x) (* (ot-off x) nps)) (- (ot-nod o) (* (ot-off o) nps))))
   "equals" (fn [x o] (boolean (and (impl/jt? o) (= :jolt.time/offset-time (impl/type-of o))
                                    (= (ot-nod x) (ot-nod o)) (= (ot-off x) (ot-off o)))))
   "hashCode" (fn [x] (ot-nod x))
   "toString" (fn [x] (str (u/iso-time-str (ot-nod x)) (offset-suffix (ot-off x))))})
;; ISO parse: "HH:mm[:ss[.fff]]±offset" — the offset is REQUIRED, as on the JVM
;; (ISO_OFFSET_TIME); its absence throws DateTimeParseException.
(defn parse-offset-time-iso [s]
  (let [s (str s)
        op (some (fn [i] (when (#{\Z \z \+ \-} (nth s i)) i)) (range (count s)))]
    (when-not op
      (throw (jolt.host/throwable "java.time.format.DateTimeParseException"
                                  (str "Text '" s "' could not be parsed: no zone offset"))))
    (ot (u/parse-hms->nano (subs s 0 op)) (z/parse-zone-offset (subs s op)))))
(statics! ["OffsetTime" "java.time.OffsetTime"]
  {"of" (fn ([tm off] (ot (l/lt-nano-of-day tm) (zo-secs* off)))
            ([h m s nano off] (ot (u/hmsn->nano (u/->long h) (u/->long m) (u/->long s) (u/->long nano)) (zo-secs* off))))
   "parse" (fn [s & _] (parse-offset-time-iso s))
   "MIN" (ot 0 (* 18 3600))
   "MAX" (ot (dec npd) (* -18 3600))})

;; --- X/from: the TemporalAccessor query --------------------------------------
;; Re-register the core from statics with the zoned/offset types added, and
;; supply the zoned-type ones — the same layering the formatter-aware parse
;; uses (the last registration of a member wins).
(defn- from-dte! [what x]
  (throw (jolt.host/throwable "java.time.DateTimeException"
                              (str "Unable to obtain " what " from TemporalAccessor: " (pr-str x)))))
(defn- t-kind [x] (when (impl/jt? x) (impl/type-of x)))
(defn- ednod-of [x]          ; [epoch-day nano-of-day] when the date+time are there
  (condp = (t-kind x)
    :jolt.time/local-date-time [(l/ldt-epoch-day x) (l/ldt-nod x)]
    :jolt.time/offset-date-time [(odt-ed x) (odt-nod x)]
    :jolt.time/zoned-date-time [(zdt-ed x) (zdt-nod x)]
    nil))
(statics! ["LocalDate" "java.time.LocalDate"]
  {"from" (fn [x] (cond (= :jolt.time/local-date (t-kind x)) x
                        (ednod-of x) (l/local-date (first (ednod-of x)))
                        :else (from-dte! "LocalDate" x)))})
(statics! ["LocalTime" "java.time.LocalTime"]
  {"from" (fn [x] (cond (= :jolt.time/local-time (t-kind x)) x
                        (= :jolt.time/offset-time (t-kind x)) (l/local-time (ot-nod x))
                        (ednod-of x) (l/local-time (second (ednod-of x)))
                        :else (from-dte! "LocalTime" x)))})
(statics! ["LocalDateTime" "java.time.LocalDateTime"]
  {"from" (fn [x] (cond (= :jolt.time/local-date-time (t-kind x)) x
                        (ednod-of x) (let [[ed nod] (ednod-of x)] (l/local-dt ed nod))
                        :else (from-dte! "LocalDateTime" x)))})
(statics! ["OffsetDateTime" "java.time.OffsetDateTime"]
  {"from" (fn [x] (condp = (t-kind x)
                    :jolt.time/offset-date-time x
                    :jolt.time/zoned-date-time (odt (zdt-ed x) (zdt-nod x) (zdt-off x))
                    (from-dte! "OffsetDateTime" x)))})
(statics! ["OffsetTime" "java.time.OffsetTime"]
  {"from" (fn [x] (condp = (t-kind x)
                    :jolt.time/offset-time x
                    :jolt.time/offset-date-time (ot (odt-nod x) (odt-off x))
                    :jolt.time/zoned-date-time (ot (zdt-nod x) (zdt-off x))
                    (from-dte! "OffsetTime" x)))})
(statics! ["ZonedDateTime" "java.time.ZonedDateTime"]
  {"from" (fn [x] (condp = (t-kind x)
                    :jolt.time/zoned-date-time x
                    :jolt.time/offset-date-time (zdt (odt-ed x) (odt-nod x) (odt-off x)
                                                     (z/zone-id (z/zo-id (odt-off x)) (odt-off x)))
                    (from-dte! "ZonedDateTime" x)))})
(statics! ["Instant" "java.time.Instant"]
  {"from" (fn [x] (condp = (t-kind x)
                    :jolt.time/instant x
                    :jolt.time/offset-date-time (inst/instant (odt->nanos x))
                    :jolt.time/zoned-date-time (inst/instant (zdt->nanos x))
                    ;; anything else keeps the core arm's permissive behavior
                    ;; (an ldt reads as UTC), which tick relies on.
                    (inst/instant (a/temporal-nanos x))))})

;; --- Clock -------------------------------------------------------------------
(defn- clock [kind ms zone base] (impl/value :jolt.time/clock {:kind kind :ms ms :zone zone :base base}))
(defn- clk-millis [c]
  (case (impl/field c :kind)
    :fixed (impl/field c :ms)
    :offset (+ (clk-millis (impl/field c :base)) (impl/field c :ms))
    :tick (let [base (impl/field c :base) dm (impl/field c :ms) m (clk-millis base)] (if (pos? dm) (* (u/floor-div m dm) dm) m))
    (System/currentTimeMillis)))
(impl/register-type! :jolt.time/clock
  {:eq (fn [a b] (= (impl/field a :ms) (impl/field b :ms))) :hash (fn [_] 0) :str (fn [_] "Clock") :cmp nil
   :classes #{"java.time.Clock" "Clock"}})
(__register-class-methods! :jolt.time/clock
  {"instant" (fn [c] (inst/instant (* (clk-millis c) 1000000))) "millis" (fn [c] (clk-millis c))
   "getZone" (fn [c] (impl/field c :zone)) "toString" (fn [_] "Clock")})
(statics! ["Clock" "java.time.Clock"]
  {"systemUTC" (fn [] (clock :system 0 (z/zone-id "Z" 0) nil))
   "systemDefaultZone" (fn [] (clock :system 0 (z/zone-id-of (z/system-zone-id)) nil))
   "system" (fn [zone] (clock :system 0 (z/zone-id-of zone) nil))
   "fixed" (fn [i zone] (clock :fixed (u/floor-div (inst/inst-nanos i) 1000000) (z/zone-id-of zone) nil))
   "offset" (fn [clk dur] (clock :offset (u/floor-div (a/dur-total-nanos dur) 1000000) (impl/field clk :zone) clk))
   "tick" (fn [clk dur] (clock :tick (u/floor-div (a/dur-total-nanos dur) 1000000) (impl/field clk :zone) clk))})

;; --- zone-aware `now` for the core value types --------------------------------
(statics! ["LocalDate" "java.time.LocalDate"]
  {"now" (fn [& args] (l/local-date (u/floor-div (first (now-local-nanos (first args))) npd)))})

(statics! ["LocalTime" "java.time.LocalTime"]
  {"now" (fn [& args] (l/local-time (u/floor-mod (first (now-local-nanos (first args))) npd)))})

(statics! ["LocalDateTime" "java.time.LocalDateTime"]
  {"now" (fn [& args] (let [[local _] (now-local-nanos (first args))]
                        (l/local-dt (u/floor-div local npd) (u/floor-mod local npd))))})

;; --- the deferred atZone / atOffset ------------------------------------------
(__register-class-methods! :jolt.time/local-date-time
  {"atZone" (fn [x zone] (zoned-of-ldt x zone))
   "atOffset" (fn [x off] (offset-of-ldt x off))})
(__register-class-methods! :jolt.time/instant
  {"atZone" (fn [x zone] (zoned-of-instant (inst/inst-nanos x) zone))
   "atOffset" (fn [x off] (let [secs (zo-secs* off) local (+ (inst/inst-nanos x) (* secs nps))]
                            (odt (u/floor-div local npd) (u/floor-mod local npd) secs)))})
(__register-class-methods! :jolt.time/local-date
  {"atStartOfDay" (fn [x & args] (if (and (seq args) (impl/jt? (first args)))
                                   (zoned-of-ldt (l/local-dt (l/ld-epoch-day x) 0) (first args))
                                   (l/local-dt (l/ld-epoch-day x) 0)))})

;; wire zoned/offset into Duration/between + the clock-aware statics
(a/register-nanos! :jolt.time/zoned-date-time zdt->nanos)
(a/register-nanos! :jolt.time/offset-date-time odt->nanos)

;; --- fill out the generic surface on ZonedDateTime / OffsetDateTime ----------
(defn- unit-nm [u] (u/upper (if (and (impl/jt? u) (= :jolt.time/chrono-unit (impl/type-of u))) (e/cu-name u) (str u))))
(defn- is-supported [ldt a]
  (cond (and (impl/jt? a) (= :jolt.time/chrono-unit (impl/type-of a))) (not= "FOREVER" (e/cu-name a))
        (and (impl/jt? a) (= :jolt.time/chrono-field (impl/type-of a))) true
        :else false))

(__register-class-methods! :jolt.time/zoned-date-time
  {"isSupported" (fn [x a] (is-supported (zdt-ldt x) a))
   "range" (fn [x f] (t/temporal-range (zdt-ldt x) (e/cf-name f)))
   "with" (fn ([x adj] (rewrap x (t/apply-adjuster (zdt-ldt x) adj)))
              ([x f v] (rewrap x (t/with-field (zdt-ldt x) (e/cf-name f) (u/->long v)))))})

(__register-class-methods! :jolt.time/offset-date-time
  {"getMonth" (fn [x] (e/month (second (civil-from-days (odt-ed x)))))
   "getDayOfWeek" (fn [x] (e/dow (inc (u/floor-mod (+ (odt-ed x) 3) 7))))
   "getMinute" (fn [x] (t/get-field (odt-ldt x) "MINUTE_OF_HOUR")) "getSecond" (fn [x] (t/get-field (odt-ldt x) "SECOND_OF_MINUTE"))
   "getNano" (fn [x] (t/get-field (odt-ldt x) "NANO_OF_SECOND"))
   "toOffsetTime" (fn [x] (ot (odt-nod x) (odt-off x)))
   "minusDays" (fn [x n] (orewrap x (l/local-dt (- (odt-ed x) (u/->long n)) (odt-nod x))))
   "plusMonths" (fn [x n] (orewrap x (t/plus-unit (odt-ldt x) (u/->long n) "MONTHS")))
   "minusMonths" (fn [x n] (orewrap x (t/plus-unit (odt-ldt x) (- (u/->long n)) "MONTHS")))
   "plusYears" (fn [x n] (orewrap x (t/plus-unit (odt-ldt x) (u/->long n) "YEARS")))
   "minusYears" (fn [x n] (orewrap x (t/plus-unit (odt-ldt x) (- (u/->long n)) "YEARS")))
   "minusHours" (fn [x n] (orewrap x (l/ldt-plus-nanos (odt-ldt x) (- (* (u/->long n) 3600 nps)))))
   "plusMinutes" (fn [x n] (orewrap x (l/ldt-plus-nanos (odt-ldt x) (* (u/->long n) 60 nps))))
   "minusMinutes" (fn [x n] (orewrap x (l/ldt-plus-nanos (odt-ldt x) (- (* (u/->long n) 60 nps)))))
   "minusSeconds" (fn [x n] (orewrap x (l/ldt-plus-nanos (odt-ldt x) (- (* (u/->long n) nps)))))
   "plusNanos" (fn [x n] (orewrap x (l/ldt-plus-nanos (odt-ldt x) (u/->long n))))
   "minusNanos" (fn [x n] (orewrap x (l/ldt-plus-nanos (odt-ldt x) (- (u/->long n)))))
   "withYear" (fn [x v] (orewrap x (t/with-field (odt-ldt x) "YEAR" (u/->long v))))
   "withMonth" (fn [x v] (orewrap x (t/with-field (odt-ldt x) "MONTH_OF_YEAR" (u/->long v))))
   "withDayOfMonth" (fn [x v] (orewrap x (t/with-field (odt-ldt x) "DAY_OF_MONTH" (u/->long v))))
   "withHour" (fn [x v] (orewrap x (t/with-field (odt-ldt x) "HOUR_OF_DAY" (u/->long v))))
   "withMinute" (fn [x v] (orewrap x (t/with-field (odt-ldt x) "MINUTE_OF_HOUR" (u/->long v))))
   "withSecond" (fn [x v] (orewrap x (t/with-field (odt-ldt x) "SECOND_OF_MINUTE" (u/->long v))))
   "withNano" (fn [x v] (orewrap x (t/with-field (odt-ldt x) "NANO_OF_SECOND" (u/->long v))))
   "truncatedTo" (fn [x unit] (orewrap x (l/local-dt (odt-ed x) (let [d (or (u/chrono-unit-nanos (unit-nm unit)) 1)] (* (quot (odt-nod x) d) d)))))
   "toEpochSecond" (fn [x] (u/floor-div (odt->nanos x) nps))
   "plus" (fn ([x amt] (orewrap x (if (a/dur? amt) (l/ldt-plus-nanos (odt-ldt x) (a/dur-total-nanos amt)) (t/plus-period (odt-ldt x) amt 1))))
              ([x n unit] (orewrap x (t/plus-unit (odt-ldt x) (u/->long n) (unit-nm unit)))))
   "minus" (fn ([x amt] (orewrap x (if (a/dur? amt) (l/ldt-plus-nanos (odt-ldt x) (- (a/dur-total-nanos amt))) (t/plus-period (odt-ldt x) amt -1))))
               ([x n unit] (orewrap x (t/plus-unit (odt-ldt x) (- (u/->long n)) (unit-nm unit)))))
   "until" (fn [x o unit] (t/unit-between (unit-nm unit) (odt-ldt x) (odt-ldt o)))
   "with" (fn ([x adj] (orewrap x (t/apply-adjuster (odt-ldt x) adj)))
              ([x f v] (orewrap x (t/with-field (odt-ldt x) (e/cf-name f) (u/->long v)))))
   "isSupported" (fn [x a] (is-supported (odt-ldt x) a))
   "range" (fn [x f] (t/temporal-range (odt-ldt x) (e/cf-name f)))
   "isEqual" (fn [x o] (= (odt->nanos x) (other-nanos o)))
   "hashCode" (fn [x] (hash (odt->nanos x)))})

;; ZDT/ODT support INSTANT_SECONDS + OFFSET_SECONDS (fields-test iterates all fields)
(let [zget (fn [x f] (case (e/cf-name f)
                       "INSTANT_SECONDS" (u/floor-div (zdt->nanos x) nps)
                       "OFFSET_SECONDS" (zdt-off x)
                       (t/get-field (zdt-ldt x) (e/cf-name f))))
      oget (fn [x f] (case (e/cf-name f)
                       "INSTANT_SECONDS" (u/floor-div (odt->nanos x) nps)
                       "OFFSET_SECONDS" (odt-off x)
                       (t/get-field (odt-ldt x) (e/cf-name f))))]
  (__register-class-methods! :jolt.time/zoned-date-time {"get" zget "getLong" zget})
  (__register-class-methods! :jolt.time/offset-date-time {"get" oget "getLong" oget}))
