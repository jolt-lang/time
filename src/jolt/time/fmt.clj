(ns jolt.time.fmt
  "DateTimeFormatter + DateTimeFormatterBuilder. A pattern engine over the library
  temporals; month/day display names are English by default, or locale-specific
  through the core jolt.host/locale-name (strftime) primitive."
  (:require [jolt.host :as host]
            [jolt.time.impl :as impl :refer [civil-from-days]]
            [jolt.time.util :as u]
            [jolt.time.local :as l]
            [jolt.time.instant :as inst]
            [jolt.time.zones :as z]))

(defn- statics! [names members] (doseq [n names] (__register-class-statics! n members)))

;; --- display names -----------------------------------------------------------
(def ^:private en-months
  ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"])
(def ^:private en-days ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"])

(defn- month-name [locale mo full?]
  (if (or (nil? locale) (= "en" locale))
    (let [n (nth en-months (dec mo))] (if full? n (subs n 0 3)))
    (or (host/locale-name locale (dec mo) 1 (if full? "%B" "%b"))
        (let [n (nth en-months (dec mo))] (if full? n (subs n 0 3))))))
(defn- day-name [locale dow full?]
  (if (or (nil? locale) (= "en" locale))
    (let [n (nth en-days (dec dow))] (if full? n (subs n 0 3)))
    (or (host/locale-name locale 0 (mod dow 7) (if full? "%A" "%a"))
        (let [n (nth en-days (dec dow))] (if full? n (subs n 0 3))))))

;; --- field extraction: [y mo d hh mi se nano dow off zid] --------------------
(defn- parts [v]
  (let [tk (impl/type-of v)
        from-ldt (fn [ed nod off zid]
                   (let [[y mo d] (civil-from-days ed)]
                     [y mo d (l/lt-hour nod) (l/lt-minute nod) (l/lt-second nod) (l/lt-nano nod)
                      (inc (u/floor-mod (+ ed 3) 7)) off zid]))]
    (condp = tk
      :jolt.time/local-date (from-ldt (l/ld-epoch-day v) 0 0 nil)
      :jolt.time/local-time (let [nod (l/lt-nano-of-day v)] [1970 1 1 (l/lt-hour nod) (l/lt-minute nod) (l/lt-second nod) (l/lt-nano nod) 4 0 nil])
      :jolt.time/local-date-time (from-ldt (l/ldt-epoch-day v) (l/ldt-nod v) 0 nil)
      :jolt.time/instant (let [n (inst/inst-nanos v) secs (u/floor-div n u/nanos-per-sec) ed (u/floor-div secs 86400) nod (+ (* (u/floor-mod secs 86400) u/nanos-per-sec) (u/floor-mod n u/nanos-per-sec))]
                           (from-ldt ed nod 0 nil))
      :jolt.time/zoned-date-time (from-ldt (impl/field v :ed) (impl/field v :nod) (impl/field v :off) (z/zid-id (impl/field v :zid)))
      :jolt.time/offset-date-time (from-ldt (impl/field v :ed) (impl/field v :nod) (impl/field v :off) nil)
      :jolt.time/offset-time (let [nod (impl/field v :nod)] [1970 1 1 (l/lt-hour nod) (l/lt-minute nod) (l/lt-second nod) (l/lt-nano nod) 4 (impl/field v :off) nil])
      nil)))

(defn- off-iso [secs colon allow-z]
  (if (and allow-z (zero? secs)) "Z"
    (let [neg (neg? secs) a (abs secs) h (quot a 3600) m (quot (mod a 3600) 60) s (mod a 60)]
      (str (if neg "-" "+") (u/pad2 h) (if colon ":" "") (u/pad2 m)
           (if (zero? s) "" (str (if colon ":" "") (u/pad2 s)))))))

(defn- scan-quote
  "Scan a '…' literal starting at the opening quote i; -> [next-i appended-out]."
  [pattern i n out]
  (if (and (< (inc i) n) (= \' (nth pattern (inc i))))
    [(+ i 2) (str out "'")]                       ; '' -> a literal quote
    (loop [j (inc i) acc out]
      (cond (>= j n) [j acc]
            (= \' (nth pattern j)) [(inc j) acc]
            :else (recur (inc j) (str acc (nth pattern j)))))))

(defn format-pattern [pattern v locale]
  (let [p (parts v)]
    (if-not p (str v)
      (let [[y mo d hh mi se nano dow off zid] p n (count pattern)]
        (letfn [(run-len [i c] (loop [j i] (if (and (< j n) (= (nth pattern j) c)) (recur (inc j)) (- j i))))]
          (loop [i 0 out ""]
            (if (>= i n) out
              (let [c (nth pattern i) k (run-len i c)]
                (cond
                  (= c \') (let [[j o] (scan-quote pattern i n out)] (recur j o))
                  (or (= c \y) (= c \Y)) (recur (+ i k) (str out (if (>= k 4) (u/pad4 y) (u/pad2 (mod y 100)))))
                  (= c \M) (recur (+ i k) (str out (cond (= k 1) (str mo) (= k 2) (u/pad2 mo) (= k 3) (month-name locale mo false) :else (month-name locale mo true))))
                  (= c \d) (recur (+ i k) (str out (if (= k 1) (str d) (u/pad2 d))))
                  (= c \E) (recur (+ i k) (str out (day-name locale dow (>= k 4))))
                  (= c \H) (recur (+ i k) (str out (if (= k 1) (str hh) (u/pad2 hh))))
                  (= c \h) (recur (+ i k) (str out (let [h12 (let [h (mod hh 12)] (if (zero? h) 12 h))] (if (= k 1) (str h12) (u/pad2 h12)))))
                  (= c \m) (recur (+ i k) (str out (if (= k 1) (str mi) (u/pad2 mi))))
                  (= c \s) (recur (+ i k) (str out (if (= k 1) (str se) (u/pad2 se))))
                  (= c \S) (recur (+ i k) (str out (u/pad-left (str (quot nano (u/pow10 (max 0 (- 9 k))))) k)))
                  (= c \a) (recur (+ i k) (str out (if (< hh 12) "AM" "PM")))
                  (= c \X) (recur (+ i k) (str out (off-iso off (>= k 3) true)))
                  (= c \x) (recur (+ i k) (str out (off-iso off (>= k 3) false)))
                  (= c \Z) (recur (+ i k) (str out (off-iso off (>= k 3) false)))
                  (or (= c \V) (= c \z)) (recur (+ i k) (str out (or zid (off-iso off true true))))
                  :else (recur (inc i) (str out c)))))))))))

;; --- DateTimeFormatter -------------------------------------------------------
(defn formatter [pattern locale] (impl/value :jolt.time/dt-formatter {:pattern pattern :locale (or locale "en")}))
(defn- fmt-pattern [f] (impl/field f :pattern))
(defn- fmt-locale [f] (impl/field f :locale))
(defn fmt? [f] (= :jolt.time/dt-formatter (impl/type-of f)))

(defn do-format [f v]
  (cond (and (impl/jt? f) (fmt? f)) (format-pattern (fmt-pattern f) v (fmt-locale f))
        (string? f) (format-pattern f v "en")
        :else (str v)))

(impl/register-type! :jolt.time/dt-formatter
  {:eq (fn [a b] (= (fmt-pattern a) (fmt-pattern b))) :hash (fn [f] (hash (fmt-pattern f)))
   :str fmt-pattern :cmp nil :classes #{"java.time.format.DateTimeFormatter" "DateTimeFormatter"}})

(defn- locale-id [l]
  (cond (and (impl/jt? l) (= :jolt.time/locale (impl/type-of l))) (impl/field l :id)
        (string? l) l :else "en"))

(__register-class-methods! :jolt.time/dt-formatter
  {"format" (fn [self v] (format-pattern (fmt-pattern self) v (fmt-locale self)))
   "withLocale" (fn [self l] (formatter (fmt-pattern self) (locale-id l)))
   "withZone" (fn [self _z] self)
   "parse" (fn [self s] (inst/instant (inst/parse-iso-instant (str s))))
   "toString" fmt-pattern})

;; register .format on every temporal
(doseq [tk [:jolt.time/local-date :jolt.time/local-time :jolt.time/local-date-time
            :jolt.time/instant :jolt.time/zoned-date-time :jolt.time/offset-date-time :jolt.time/offset-time]]
  (__register-class-methods! tk {"format" (fn [self f] (do-format f self))}))

;; java.util.Locale, java.time.format.FormatStyle
(statics! ["Locale" "java.util.Locale"]
  {"ENGLISH" (impl/value :jolt.time/locale {:id "en"}) "US" (impl/value :jolt.time/locale {:id "en"})
   "FRENCH" (impl/value :jolt.time/locale {:id "fr"}) "GERMAN" (impl/value :jolt.time/locale {:id "de"})})
(__register-class-ctor! "Locale" (fn [lang & _] (impl/value :jolt.time/locale {:id (str lang)})))
(__register-class-ctor! "java.util.Locale" (fn [lang & _] (impl/value :jolt.time/locale {:id (str lang)})))
(impl/register-type! :jolt.time/locale
  {:eq (fn [a b] (= (impl/field a :id) (impl/field b :id))) :hash (fn [l] (hash (impl/field l :id)))
   :str (fn [l] (impl/field l :id)) :cmp nil :classes #{"java.util.Locale" "Locale"}})

(def ^:private style-patterns
  {:date {"SHORT" "M/d/yy" "MEDIUM" "MMM d, yyyy" "LONG" "MMMM d, yyyy" "FULL" "EEEE, MMMM d, yyyy"}
   :time {"SHORT" "h:mm a" "MEDIUM" "h:mm:ss a" "LONG" "h:mm:ss a" "FULL" "h:mm:ss a"}
   :datetime {"SHORT" "M/d/yy, h:mm a" "MEDIUM" "MMM d, yyyy, h:mm:ss a" "LONG" "MMMM d, yyyy, h:mm:ss a" "FULL" "EEEE, MMMM d, yyyy, h:mm:ss a"}})
(statics! ["FormatStyle" "java.time.format.FormatStyle"]
  (into {} (map (fn [s] [s (impl/value :jolt.time/format-style {:style s})]) ["SHORT" "MEDIUM" "LONG" "FULL"])))
(impl/register-type! :jolt.time/format-style
  {:eq (fn [a b] (= (impl/field a :style) (impl/field b :style))) :hash (fn [s] (hash (impl/field s :style)))
   :str (fn [s] (impl/field s :style)) :cmp nil :classes #{"java.time.format.FormatStyle" "FormatStyle"}})
(defn- style-of [fs] (impl/field fs :style))
(defn- style-fmt [kind fs] (formatter (get-in style-patterns [kind (style-of fs)] "yyyy-MM-dd HH:mm:ss") "en"))

(statics! ["DateTimeFormatter" "java.time.format.DateTimeFormatter"]
  {"ofPattern" (fn [p & r] (formatter (str p) (if (seq r) (locale-id (first r)) "en")))
   "ISO_LOCAL_DATE" (formatter "yyyy-MM-dd" "en") "ISO_LOCAL_TIME" (formatter "HH:mm:ss" "en")
   "ISO_LOCAL_DATE_TIME" (formatter "yyyy-MM-dd'T'HH:mm:ss" "en")
   "ISO_DATE" (formatter "yyyy-MM-dd" "en") "ISO_TIME" (formatter "HH:mm:ss" "en")
   "ISO_DATE_TIME" (formatter "yyyy-MM-dd'T'HH:mm:ss" "en")
   "ISO_INSTANT" (formatter "yyyy-MM-dd'T'HH:mm:ssX" "en")
   "ISO_OFFSET_DATE_TIME" (formatter "yyyy-MM-dd'T'HH:mm:ssXXX" "en")
   "BASIC_ISO_DATE" (formatter "yyyyMMdd" "en")
   "RFC_1123_DATE_TIME" (formatter "EEE, dd MMM yyyy HH:mm:ss Z" "en")
   "ofLocalizedDate" (fn [fs] (style-fmt :date fs))
   "ofLocalizedTime" (fn [fs] (style-fmt :time fs))
   "ofLocalizedDateTime" (fn [fs] (style-fmt :datetime fs))})

;; --- DateTimeFormatterBuilder (minimal) --------------------------------------
(defn- builder [] (impl/value :jolt.time/dtf-builder {:pattern (atom "")}))
(defn- b-append! [b s] (swap! (impl/field b :pattern) str s) b)
(__register-class-ctor! "DateTimeFormatterBuilder" (fn [& _] (builder)))
(__register-class-ctor! "java.time.format.DateTimeFormatterBuilder" (fn [& _] (builder)))
(impl/register-type! :jolt.time/dtf-builder
  {:eq (fn [a b] (identical? a b)) :hash (fn [_] 0) :str (fn [_] "DateTimeFormatterBuilder") :cmp nil
   :classes #{"java.time.format.DateTimeFormatterBuilder" "DateTimeFormatterBuilder"}})
(__register-class-methods! :jolt.time/dtf-builder
  {"appendPattern" (fn [b p] (b-append! b (str p)))
   "appendLiteral" (fn [b s] (b-append! b (str "'" s "'")))
   "parseCaseInsensitive" (fn [b] b) "parseLenient" (fn [b] b)
   "optionalStart" (fn [b] b) "optionalEnd" (fn [b] b)
   "toFormatter" (fn [b & _] (formatter @(impl/field b :pattern) "en"))})
