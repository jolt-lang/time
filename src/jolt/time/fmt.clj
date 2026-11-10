(ns jolt.time.fmt
  "DateTimeFormatter + DateTimeFormatterBuilder. A pattern engine over the library
  temporals; localized patterns and month/day display names come from the
  bundled jolt.time.locale-data tables (measured from the reference JVM), with
  the core jolt.host/locale-name (strftime) primitive as a last resort."
  (:require [clojure.string :as str]
            [jolt.host :as host]
            [jolt.time.impl :as impl :refer [civil-from-days days-from-civil]]
            [jolt.time.locale-data :as ld]
            [jolt.time.locale-names :as ln]
            [jolt.time.number-data :as nd]
            [jolt.time.util :as u]
            [jolt.time.local :as l]
            [jolt.time.year :as y]
            [jolt.time.instant :as inst]
            [jolt.time.zones :as z]
            [jolt.time.zoned :as zd]))

(defn- statics! [names members] (doseq [n names] (__register-class-statics! n members)))

;; --- display names -----------------------------------------------------------
(def ^:private en-months
  ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"])
(def ^:private en-days ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"])

(defn- month-name [locale mo full?]
  ;; bundled tables first: they are right everywhere; strftime depends on the OS
  ;; having the locale installed and silently falls back to English when it does not
  (if-let [names (ld/months locale full?)]
    (nth names (dec mo))
    (or (host/locale-name locale (dec mo) 1 (if full? "%B" "%b"))
        (let [n (nth en-months (dec mo))] (if full? n (subs n 0 3))))))
(defn- day-name [locale dow full?]
  (if-let [names (ld/days locale full?)]
    (nth names (dec dow))
    (or (host/locale-name locale 0 (mod dow 7) (if full? "%A" "%a"))
        (let [n (nth en-days (dec dow))] (if full? n (subs n 0 3))))))

;; --- field extraction: [y mo d hh mi se nano dow off zid] --------------------
;; off is nil for a value with no offset (the local types), which is what the
;; ISO constants' optional offset sections test; a pattern letter that asks for
;; one anyway reads it as zero.
(defn- parts [v]
  (let [tk (impl/type-of v)
        from-ldt (fn [ed nod off zid]
                   (let [[y mo d] (civil-from-days ed)]
                     [y mo d (l/lt-hour nod) (l/lt-minute nod) (l/lt-second nod) (l/lt-nano nod)
                      (inc (u/floor-mod (+ ed 3) 7)) off zid]))]
    (if (and (not tk) (instance? java.util.Date v))
      ;; the #inst/Date layer's value (UTC), so its .format renders like an instant
      (let [ms (.getTime v) secs (u/floor-div ms 1000)
            ed (u/floor-div secs 86400) nod (* (u/floor-mod secs 86400) u/nanos-per-sec)]
        (from-ldt ed nod 0 nil))
    (condp = tk
      :jolt.time/local-date (from-ldt (l/ld-epoch-day v) 0 nil nil)
      :jolt.time/local-time (let [nod (l/lt-nano-of-day v)] [1970 1 1 (l/lt-hour nod) (l/lt-minute nod) (l/lt-second nod) (l/lt-nano nod) 4 nil nil])
      :jolt.time/local-date-time (from-ldt (l/ldt-epoch-day v) (l/ldt-nod v) nil nil)
      :jolt.time/instant (let [n (inst/inst-nanos v) secs (u/floor-div n u/nanos-per-sec) ed (u/floor-div secs 86400) nod (+ (* (u/floor-mod secs 86400) u/nanos-per-sec) (u/floor-mod n u/nanos-per-sec))]
                           (from-ldt ed nod 0 nil))
      :jolt.time/zoned-date-time (from-ldt (impl/field v :ed) (impl/field v :nod) (impl/field v :off) (z/zid-id (impl/field v :zid)))
      :jolt.time/offset-date-time (from-ldt (impl/field v :ed) (impl/field v :nod) (impl/field v :off) nil)
      :jolt.time/offset-time (let [nod (impl/field v :nod)] [1970 1 1 (l/lt-hour nod) (l/lt-minute nod) (l/lt-second nod) (l/lt-nano nod) 4 (impl/field v :off) nil])
      nil))))

;; The instant a value names, in epoch nanos, or nil when it names none.
(defn- instant-nanos [v]
  (let [tk (impl/type-of v)
        of-fields (fn [] (- (+ (* (impl/field v :ed) u/nanos-per-day) (impl/field v :nod))
                            (* (impl/field v :off) u/nanos-per-sec)))]
    (cond (and (not tk) (instance? java.util.Date v)) (* (.getTime v) 1000000)
          (= tk :jolt.time/instant) (inst/inst-nanos v)
          (= tk :jolt.time/zoned-date-time) (of-fields)
          (= tk :jolt.time/offset-date-time) (of-fields)
          :else nil)))

;; A formatter's override zone (withZone), as java.time applies it before
;; printing: a value that names an instant is read as a ZonedDateTime in the
;; zone; anything else keeps its fields and answers the zone letters with the
;; zone.
(defn- in-zone [v zone]
  (if-let [n (and zone (instant-nanos v))]
    (zd/zoned-of-instant n zone)
    v))

(defn- offset-id? [id] (and (pos? (count id)) (#{\+ \- \Z} (nth id 0))))

;; The zone's short name (z, zz, zzz): an offset zone is its own id; a region
;; reads its abbreviation at the instant through core's SimpleDateFormat
;; (libc's names), or, with no instant to place it, its standard-time name, as
;; java.time does. nil when nothing can be said.
(defn- zone-short-name [zid epoch-ms]
  (if (offset-id? zid)
    zid
    (try
      (let [tz (java.util.TimeZone/getTimeZone zid)
            ms (or epoch-ms
                   ;; standard time: the smaller of the two solstice offsets
                   (let [jan 1704110400000 jul 1719835200000]
                     (if (<= (.getOffset tz jan) (.getOffset tz jul)) jan jul)))
            f (java.text.SimpleDateFormat. "zzz")]
        (.setTimeZone f tz)
        (.format f (java.util.Date. ms)))
      (catch Exception _ nil))))

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

;; The ISO_* formatters print the second's fraction the way java.time's
;; ISO_LOCAL_TIME does -- omitted when zero, else as many digits as carry a
;; value, up to nine -- and a pattern's fixed-width S never does; so the rule
;; is a flag on the formatter (`iso-frac`) that the seconds letter reads.
;; ISO_INSTANT is the exception: it prints the fraction in whole groups of three
;; (".500", ".000120"), which is the :instant mode.
(defn- iso-fraction [nano mode]
  (if (zero? nano)
    ""
    (let [s (u/pad-left (str nano) 9)
          last-digit (loop [i 8] (if (= \0 (nth s i)) (recur (dec i)) i))
          width (if (= :instant mode) (* 3 (quot (+ last-digit 3) 3)) (inc last-digit))]
      (str "." (subs s 0 width)))))
;; Two digits for a field in 0..99. The shared util's pad2 builds its padding
;; with apply/repeat, which was most of a format's cost.
(defn- pad2 [n] (if (and (>= n 0) (< n 10)) (str "0" n) (str n)))

(defn- render-parts [pattern p locale iso-frac]
  (let [[y mo d hh mi se nano dow off zid] p n (count pattern) off0 (or off 0)
        sb (StringBuilder.)
        emit (fn [i k x] (.append sb x) (+ i k))]
    (letfn [(run-len [i c] (loop [j i] (if (and (< j n) (= (nth pattern j) c)) (recur (inc j)) (- j i))))
            ;; the instant, for a zone's short name; nil for a value with no offset
            (epoch-ms [] (when off
                           (- (* (+ (* (days-from-civil y mo d) 86400) (* hh 3600) (* mi 60) se) 1000)
                              (* off 1000))))]
      (loop [i 0]
        (if (>= i n)
          (.toString sb)
          (let [c (nth pattern i) k (run-len i c)]
            (recur
              (cond
                (= c \') (let [[j o] (scan-quote pattern i n "")] (.append sb o) j)
                ;; java.time year semantics: y is the full year, yy the reduced
                ;; two-digit form, yyy+ padded (CLDR localized patterns use bare y)
                (or (= c \y) (= c \Y)) (emit i k (cond (= k 1) (str y) (= k 2) (pad2 (mod y 100)) :else (u/pad4 y)))
                (= c \M) (emit i k (cond (= k 1) (str mo) (= k 2) (pad2 mo) (= k 3) (month-name locale mo false) :else (month-name locale mo true)))
                (= c \d) (emit i k (if (= k 1) (str d) (pad2 d)))
                (= c \E) (emit i k (day-name locale dow (>= k 4)))
                (= c \H) (emit i k (if (= k 1) (str hh) (pad2 hh)))
                (= c \h) (emit i k (let [h12 (let [h (mod hh 12)] (if (zero? h) 12 h))] (if (= k 1) (str h12) (pad2 h12))))
                (= c \m) (emit i k (if (= k 1) (str mi) (pad2 mi)))
                (= c \s) (do (.append sb (if (= k 1) (str se) (pad2 se)))
                             (emit i k (if iso-frac (iso-fraction nano iso-frac) "")))
                (= c \S) (emit i k (u/pad-left (str (quot nano (u/pow10 (max 0 (- 9 k))))) k))
                (= c \a) (emit i k (if (< hh 12) "AM" "PM"))
                (= c \X) (emit i k (off-iso off0 (>= k 3) true))
                (= c \x) (emit i k (off-iso off0 (>= k 3) false))
                (= c \Z) (emit i k (off-iso off0 (>= k 3) false))
                (and (= c \z) (< k 4) zid) (emit i k (or (zone-short-name zid (epoch-ms)) zid))
                (or (= c \V) (= c \z)) (emit i k (or zid (off-iso off0 true true)))
                :else (emit i 1 (str c))))))))))

(defn format-pattern
  ([pattern v locale] (format-pattern pattern v locale nil))
  ([pattern v locale iso-frac]
   (if-let [p (parts v)] (render-parts pattern p locale iso-frac) (str v))))

;; --- DateTimeFormatter -------------------------------------------------------
;; A formatter is {:pattern :locale} plus, where they apply: :style (a localized
;; formatter, resolved per locale), :iso (the ISO fraction mode), :zone (the
;; withZone override), and for the ISO constants whose printed shape a pattern
;; string cannot spell, :print (the pattern printed, :pattern being what parses
;; and what toString shows) and :tail (an optional section after it).
(def ^:private fmt-keys [:pattern :style :iso :locale :zone :print :tail])
(defn- fmt-value [m] (impl/value :jolt.time/dt-formatter (into {} (remove (comp nil? val)) m)))
(defn- fmt-fields [f] (into {} (map (fn [k] [k (impl/field f k)])) fmt-keys))
(defn formatter [pattern locale] (fmt-value {:pattern pattern :locale (or locale "en")}))
(defn- iso-formatter
  ([pattern] (iso-formatter pattern true))
  ([pattern mode] (fmt-value {:pattern pattern :locale "en" :iso mode})))
(defn- fmt-pattern [f]
  (or (impl/field f :pattern)
      (let [[kind style] (impl/field f :style)]
        (or (ld/pattern (impl/field f :locale) kind style) "yyyy-MM-dd HH:mm:ss"))))
(defn- fmt-locale [f] (impl/field f :locale))
(defn fmt? [f] (= :jolt.time/dt-formatter (impl/type-of f)))

;; The optional sections of the ISO constants: the offset when the value has
;; one, and the region in brackets when its zone is a region rather than an
;; offset. RFC 1123 names a zero offset GMT.
(defn- render-tail [tail off zid]
  (let [region (if (and zid (not (offset-id? zid))) (str "[" zid "]") "")]
    (case tail
      :zone region
      :offset (if off (off-iso off true true) "")
      :offset-zone (if off (str (off-iso off true true) region) "")
      :rfc (let [o (or off 0)] (str " " (if (zero? o) "GMT" (off-iso o false false))))
      "")))

(def ^:private utc-zone "Z")

(defn- format-with [f v]
  (let [iso (impl/field f :iso)
        zone (impl/field f :zone)
        ;; ISO_INSTANT prints the instant, so whatever the zone it reads in UTC
        v (if (= :instant iso) (in-zone v utc-zone) (in-zone v zone))
        p (parts v)]
    (if-not p
      (str v)
      (let [p (if (and zone (nil? (nth p 9))) (assoc p 9 (first (z/resolve-zone zone))) p)]
        (str (render-parts (or (impl/field f :print) (fmt-pattern f)) p (fmt-locale f) iso)
             (render-tail (impl/field f :tail) (nth p 8) (nth p 9)))))))

(defn do-format [f v]
  (cond (and (impl/jt? f) (fmt? f)) (format-with f v)
        (string? f) (format-pattern f v "en")
        :else (str v)))

(impl/register-type! :jolt.time/dt-formatter
  {:eq (fn [a b] (= (fmt-pattern a) (fmt-pattern b))) :hash (fn [f] (hash (fmt-pattern f)))
   :str fmt-pattern :cmp nil :classes #{"java.time.format.DateTimeFormatter" "DateTimeFormatter"}})

(defn- locale-id [l]
  (cond (and (impl/jt? l) (= :jolt.time/locale (impl/type-of l))) (impl/field l :id)
        (string? l) l :else "en"))

;; .parse below calls the pattern parsers defined further down in this file.
(declare parse-with-pattern parse-fields-strict parse-zoned-strict strict-parseable?
         parsed-value accessor-of zoned-in)

(__register-class-methods! :jolt.time/dt-formatter
  {"format" (fn [self v] (format-with self v))
   "withLocale" (fn [self l] (fmt-value (assoc (fmt-fields self) :locale (locale-id l))))
   "withZone" (fn [self z] (fmt-value (assoc (fmt-fields self) :zone z)))
   "getZone" (fn [self] (impl/field self :zone))
   "getLocale" (fn [self] (impl/field self :locale))
   "parse" (fn
             ([self s] (parsed-value self (str s)))
             ;; (.parse fmt s TemporalQuery) — parse STRICTLY (malli's -safe
             ;; decode relies on malformed input THROWING), build the accessor,
             ;; and let the query pull the type it wants ((X/from acc) in
             ;; malli's queries). ISO_ZONED_DATE_TIME bypasses the pattern
             ;; walker — its zone-bracket + wide-fraction shape parses as ISO.
             ([self s query]
              (let [str-s (str s)
                    zone (impl/field self :zone)
                    acc (cond
                          (= :zoned (impl/field self :iso))
                          (let [x (parse-zoned-strict str-s)]
                            (if zone (zoned-in x zone) x))
                          (strict-parseable? (fmt-pattern self))
                          (accessor-of (parse-fields-strict (fmt-pattern self) str-s) zone true)
                          :else
                          (accessor-of (parse-with-pattern (fmt-pattern self) str-s) zone true))]
                (.queryFrom query acc))))
   "toString" fmt-pattern})

;; register .format on every temporal
(doseq [tk [:jolt.time/local-date :jolt.time/local-time :jolt.time/local-date-time
            :jolt.time/instant :jolt.time/zoned-date-time :jolt.time/offset-date-time :jolt.time/offset-time]]
  (__register-class-methods! tk {"format" (fn [self f] (do-format f self))}))

;; java.util.Locale, java.time.format.FormatStyle
;; java.util.Locale lives here, not in jolt core: it is only meaningful for
;; formatting, which is this library (RFC 0008). Carry the constants and
;; getDefault/setDefault the surface needs (tick asks for Locale/getDefault); the
;; base default is English.
;;
;; A Locale is its language, script, country and variant. Its :id, the key its
;; data is registered under, is the BCP-47-style join of the first three
;; ("en-US", "zh-Hant-TW", "" for ROOT); jolt core narrows a missed id to its
;; language. It prints as the JDK's toString does: language_COUNTRY_VARIANT, then
;; _#Script ("zh_TW_#Hant", "sr__#Latn").
(defn- mk-locale [lang script country variant]
  (impl/value :jolt.time/locale
              {:id (str/join "-" (remove empty? [lang script country]))
               :lang lang :script script :country country :variant variant}))
(defn- lf [l k] (or (impl/field l k) ""))
(defn- locale-of
  ([lang] (locale-of lang "" ""))
  ([lang country] (locale-of lang country ""))
  ([lang country variant]
   (mk-locale (str/lower-case (str lang)) "" (str/upper-case (str country)) (str variant))))
(defn- bcp-variant? [v] (boolean (re-matches #"(?i)[0-9a-z]{5,8}|[0-9][0-9a-z]{3}" v)))
(defn- parse-tag [tag]
  (let [[lang & more] (str/split (str tag) #"-")
        lang (str/lower-case (or lang ""))
        lang (if (= lang "und") "" lang)]
    (loop [xs more script "" country "" variants []]
      (let [x (first xs)]
        (cond
          (nil? x) (mk-locale lang script country (str/join "_" variants))
          (and (= "x" (str/lower-case x)) (= "lvariant" (some-> (second xs) str/lower-case)))
          (mk-locale lang script country (str/join "_" (concat variants (drop 2 xs))))
          (and (empty? script) (empty? country) (empty? variants) (re-matches #"(?i)[a-z]{4}" x))
          (recur (rest xs) (str (str/upper-case (subs x 0 1)) (str/lower-case (subs x 1))) country variants)
          (and (empty? country) (empty? variants) (re-matches #"(?i)[a-z]{2}|[0-9]{3}" x))
          (recur (rest xs) script (str/upper-case x) variants)
          (bcp-variant? x) (recur (rest xs) script country (conj variants x))
          ;; an extension or private-use section: not modeled, ends the parse
          :else (mk-locale lang script country (str/join "_" variants)))))))
(defn- locale-string [l]
  (let [lang (lf l :lang) script (lf l :script) country (lf l :country) variant (lf l :variant)]
    (str lang
         (when (or (seq country) (seq variant) (seq script)) (str "_" country))
         (when (seq variant) (str "_" variant))
         (when (seq script) (str "_#" script)))))
(defn- locale-tag [l]
  (let [lang (lf l :lang) variant (lf l :variant)
        base (str/join "-" (remove empty? [(if (empty? lang) "und" lang) (lf l :script) (lf l :country)]))]
    (cond (empty? variant) base
          (every? bcp-variant? (str/split variant #"_")) (str base "-" (str/replace variant "_" "-"))
          :else (str base "-x-lvariant-" (str/replace variant "_" "-")))))
(statics! ["Locale" "java.util.Locale"]
  {"getDefault" (fn [& _] (locale-of "en"))
   "setDefault" (fn [_] nil)
   "forLanguageTag" parse-tag
   "of" locale-of
   "ENGLISH" (locale-of "en") "US" (locale-of "en" "US")
   "FRENCH" (locale-of "fr") "FRANCE" (locale-of "fr" "FR")
   "GERMAN" (locale-of "de") "GERMANY" (locale-of "de" "DE")
   "ITALIAN" (locale-of "it") "ITALY" (locale-of "it" "IT")
   "JAPANESE" (locale-of "ja") "JAPAN" (locale-of "ja" "JP")
   "KOREAN" (locale-of "ko") "KOREA" (locale-of "ko" "KR")
   "CHINESE" (locale-of "zh") "CHINA" (locale-of "zh" "CN")
   "SIMPLIFIED_CHINESE" (locale-of "zh" "CN") "TRADITIONAL_CHINESE" (locale-of "zh" "TW")
   "PRC" (locale-of "zh" "CN") "TAIWAN" (locale-of "zh" "TW")
   "UK" (locale-of "en" "GB") "CANADA" (locale-of "en" "CA")
   "CANADA_FRENCH" (locale-of "fr" "CA") "ROOT" (locale-of "")})
;; (Locale. lang), (Locale. lang country), (Locale. lang country variant). The
;; one-arg ctor's argument is a *language* subtag, so "en_US" stays malformed
;; and resolves to ROOT at lookup time
(__register-class-ctor! "Locale" locale-of)
(__register-class-ctor! "java.util.Locale" locale-of)
(impl/register-type! :jolt.time/locale
  {:eq (fn [a b] (and (= (impl/field a :id) (impl/field b :id)) (= (lf a :variant) (lf b :variant))))
   :hash (fn [l] (hash [(impl/field l :id) (lf l :variant)]))
   :str locale-string :cmp nil :classes #{"java.util.Locale" "Locale"}})
;; Display names, in a display locale (the default's, en, when none is given):
;; each part's name from that locale's table, or the code itself when it has
;; none, and getDisplayName the language followed by the script, country and
;; variants in parentheses, joined the way that locale joins a list.
(defn- display-in [args] (if (seq args) (impl/field (first args) :id) "en"))
(defn- dn-part [names kind code] (if (empty? code) "" (get-in names [kind code] code)))
(defn- dn-variants [names v]
  (str/join (:separator names) (map #(dn-part names :variants %) (remove empty? (str/split v #"_")))))
(defn- display-name [l in]
  (let [names (ln/names in)
        lang (dn-part names :languages (lf l :lang))
        quals (remove empty? [(dn-part names :scripts (lf l :script))
                              (dn-part names :countries (lf l :country))
                              (dn-variants names (lf l :variant))])]
    ;; with no language the first qualifier stands in front (und-Latn-US is
    ;; "Latin (United States)")
    (let [[head & more] (if (empty? lang) quals (cons lang quals))]
      (if (seq more) (str head " (" (str/join (:separator names) more) ")") (or head "")))))
(__register-class-methods! :jolt.time/locale
  {"toString" locale-string
   "toLanguageTag" locale-tag
   "getLanguage" (fn [l] (lf l :lang))
   "getScript" (fn [l] (lf l :script))
   "getCountry" (fn [l] (lf l :country))
   "getVariant" (fn [l] (lf l :variant))
   "getDisplayLanguage" (fn [l & in] (dn-part (ln/names (display-in in)) :languages (lf l :lang)))
   "getDisplayCountry" (fn [l & in] (dn-part (ln/names (display-in in)) :countries (lf l :country)))
   "getDisplayScript" (fn [l & in] (dn-part (ln/names (display-in in)) :scripts (lf l :script)))
   "getDisplayVariant" (fn [l & in] (dn-variants (ln/names (display-in in)) (lf l :variant)))
   "getDisplayName" (fn [l & in] (display-name l (display-in in)))})

;; jolt core's java.text.SimpleDateFormat renders MMM/MMMM/EEE/EEEE through a
;; :date-names extension point and carries ROOT alone, because core has no locale
;; data of its own. Hand it the bundled tables so an explicit locale renders in its
;; own language there too, not only through this library's DateTimeFormatter.
;;
;; Guarded on the POINT existing, so a jolt that declares fewer points still loads
;; this library and simply keeps ROOT names in SimpleDateFormat. The guard is
;; deliberately narrow: it swallows the one "not declared" case and rethrows
;; anything else, so a genuine error in a provider still surfaces. The registering
;; fn itself, jolt.host/register-extension!, is referenced statically: it has been
;; part of core since extension points arrived (v0.5.13), and looking it up with
;; `resolve` at load time made every app that requires this namespace resolve a
;; var by name — which is the one thing `jolt build --tree-shake` cannot follow,
;; so it kept every def and the compiler image for all of them.
;; The same applies to the other two locale-sensitive surfaces core declares:
;; :number-symbols (String/format's decimal and grouping separators) and
;; :currency-data (NumberFormat/getCurrencyInstance). All three are fed from the
;; bundled tables here, because core carries ROOT alone for each.
;;
;; A point core does not declare is skipped rather than failing the load, so this
;; library keeps working against a jolt that has only some of them.
(defn- register-point! [reg point entries]
  (try
    ;; under the id and, for language-COUNTRY, under the Locale's printed form
    ;; too ("en_US"): a jolt before 0.9 looks the data up by that
    (doseq [[id data] entries :when (not= id "")
            k (distinct [id (str/replace id "-" "_")])]
      (reg point k data))
    true
    (catch Exception e
      ;; an undeclared point, or a field an older jolt's point does not declare
      ;; (the caller falls back to the fields it has)
      (if (re-find #"no extension point|has unknown field" (or (ex-message e) ""))
        false
        (throw e)))))

(defn- register-locale-points! []
  (let [reg host/register-extension!]
    ;; the narrow and standalone forms ride on :date-names: a jolt whose point
    ;; does not declare those fields refuses the entry, so it falls back to the
    ;; four it always had
    (when-not (register-point! reg :date-names
                (for [[id spec] ld/locales]
                  [id (merge {:months (:months spec) :months-short (:months-short spec)
                              :days (:days spec) :days-short (:days-short spec)}
                             (get-in ld/display-names [id :text]))]))
      (register-point! reg :date-names
        (for [[id spec] ld/locales]
          [id {:months (:months spec) :months-short (:months-short spec)
               :days (:days spec) :days-short (:days-short spec)}])))
    (register-point! reg :field-names
      (for [[id spec] ld/display-names] [id {:names (:field-names spec)}]))
    (register-point! reg :number-symbols nd/number-symbols)
    (register-point! reg :currency-data nd/currency)
    true))

;; register-point! already tolerates an undeclared point per point, so this only
;; has to run once.
(defonce ^:private locale-points-registered (boolean (register-locale-points!)))

;; TextStyle: the six styles a text field renders in, in JDK declaration order,
;; declared through jolt.host/register-enum! as FormatStyle is below. Month,
;; DayOfWeek and ChronoField's getDisplayName take one (core java.time), and read
;; this library's names through the :date-names and :field-names points.
(def ^:private text-style-names ["FULL" "FULL_STANDALONE" "SHORT" "SHORT_STANDALONE" "NARROW" "NARROW_STANDALONE"])
(def ^:private text-style-ordinal (zipmap text-style-names (range)))
(def ^:private text-styles
  (mapv (fn [s] (impl/value :jolt.time/text-style {:style s})) text-style-names))
(defn- text-style [n] (nth text-styles (text-style-ordinal n)))
(defn- text-style-name [s] (impl/field s :style))
(defn- standalone? [s] (str/ends-with? (text-style-name s) "_STANDALONE"))
(impl/register-type! :jolt.time/text-style
  {:eq (fn [a b] (= (text-style-name a) (text-style-name b))) :hash (fn [s] (hash (text-style-name s)))
   :str text-style-name
   :class "java.time.format.TextStyle"
   :enum? true
   :cmp (fn [a b] (- (text-style-ordinal (text-style-name a)) (text-style-ordinal (text-style-name b))))
   :classes #{"java.time.format.TextStyle" "TextStyle"
              "java.lang.Comparable" "Comparable" "java.lang.Enum" "Enum"}})
(__register-class-methods! :jolt.time/text-style
  {"isStandalone" (fn [s] (standalone? s))
   "asStandalone" (fn [s] (if (standalone? s) s (text-style (str (text-style-name s) "_STANDALONE"))))
   "asNormal" (fn [s] (if (standalone? s) (text-style (subs (text-style-name s) 0 (- (count (text-style-name s)) 11))) s))})
(if-let [register-enum! (resolve 'jolt.host/register-enum!)]
  (register-enum! "java.time.format.TextStyle" (mapv vector text-style-names text-styles))
  (statics! ["TextStyle" "java.time.format.TextStyle"] (zipmap text-style-names text-styles)))

;; FormatStyle is an enum: its constants in JDK declaration order, declared
;; through jolt.host/register-enum! (values, valueOf, ordinal, compareTo,
;; EnumSet, the #object print form). An older jolt without register-enum!
;; still gets the constants as plain statics.
(def ^:private style-names ["FULL" "LONG" "MEDIUM" "SHORT"])
(def ^:private style-key {"SHORT" :short "MEDIUM" :medium "LONG" :long "FULL" :full})
(def ^:private style-ordinal (zipmap style-names (range)))
(def ^:private format-styles
  (mapv (fn [s] (impl/value :jolt.time/format-style {:style s})) style-names))
(impl/register-type! :jolt.time/format-style
  {:eq (fn [a b] (= (impl/field a :style) (impl/field b :style))) :hash (fn [s] (hash (impl/field s :style)))
   :str (fn [s] (impl/field s :style))
   :enum? true
   ;; Enum.compareTo: the ordinal difference
   :cmp (fn [a b] (- (style-ordinal (impl/field a :style)) (style-ordinal (impl/field b :style))))
   :classes #{"java.time.format.FormatStyle" "FormatStyle"
              "java.lang.Comparable" "Comparable" "java.lang.Enum" "Enum"}})
(if-let [register-enum! (resolve 'jolt.host/register-enum!)]
  (register-enum! "java.time.format.FormatStyle" (mapv vector style-names format-styles))
  (statics! ["FormatStyle" "java.time.format.FormatStyle"] (zipmap style-names format-styles)))
(defn- style-of [fs] (impl/field fs :style))
;; ofLocalized* defers pattern resolution: the pattern depends on the locale,
;; which .withLocale supplies after construction, so the value carries
;; [kind style] and fmt-pattern resolves it per locale at use time.
(defn- style-fmt [kind fs]
  (impl/value :jolt.time/dt-formatter {:style [kind (style-key (style-of fs))] :locale "en"}))

(statics! ["DateTimeFormatter" "java.time.format.DateTimeFormatter"]
  {"ofPattern" (fn [p & r] (formatter (str p) (if (seq r) (locale-id (first r)) "en")))
   "ISO_LOCAL_DATE" (formatter "yyyy-MM-dd" "en") "ISO_LOCAL_TIME" (iso-formatter "HH:mm:ss")
   ;; the ISO constants carry :iso, so the second's fraction prints as
   ;; java.time's do (see iso-fraction); the strict parser already takes an
   ;; optional fraction after the seconds when the pattern spells no S
   "ISO_LOCAL_DATE_TIME" (iso-formatter "yyyy-MM-dd'T'HH:mm:ss")
   ;; ISO_DATE, ISO_TIME and ISO_DATE_TIME add the offset when the value has
   ;; one, and ISO_DATE_TIME the region after it
   "ISO_DATE" (fmt-value {:pattern "yyyy-MM-dd" :locale "en" :tail :offset})
   "ISO_TIME" (fmt-value {:pattern "HH:mm:ss" :locale "en" :iso true :tail :offset})
   "ISO_DATE_TIME" (fmt-value {:pattern "yyyy-MM-dd'T'HH:mm:ss" :locale "en" :iso true :tail :offset-zone})
   "ISO_INSTANT" (iso-formatter "yyyy-MM-dd'T'HH:mm:ssX" :instant)
   "ISO_OFFSET_DATE_TIME" (iso-formatter "yyyy-MM-dd'T'HH:mm:ssXXX")
   "ISO_OFFSET_TIME" (iso-formatter "HH:mm:ssXXX")
   "ISO_OFFSET_DATE" (formatter "yyyy-MM-ddXXX" "en")
   "ISO_ZONED_DATE_TIME" (fmt-value {:pattern "yyyy-MM-dd'T'HH:mm:ssXXX'['VV']'" :locale "en" :iso :zoned
                                     ;; the bracketed region only for a region zone
                                     :print "yyyy-MM-dd'T'HH:mm:ssXXX" :tail :zone})
   "ISO_ORDINAL_DATE" (formatter "yyyy-DDD" "en")
   "ISO_WEEK_DATE" (formatter "yyyy-'W'ww-e" "en")
   "BASIC_ISO_DATE" (formatter "yyyyMMdd" "en")
   "RFC_1123_DATE_TIME" (fmt-value {:pattern "EEE, dd MMM yyyy HH:mm:ss Z" :locale "en"
                                    ;; a one- or two-digit day, and GMT for a zero offset
                                    :print "EEE, d MMM yyyy HH:mm:ss" :tail :rfc})
   "ofLocalizedDate" (fn [fs] (style-fmt :date fs))
   "ofLocalizedTime" (fn [fs] (style-fmt :time fs))
   "ofLocalizedDateTime" (fn [fs] (style-fmt :date-time fs))})

;; --- DateTimeFormatterBuilder (minimal) --------------------------------------
(defn- builder [] (impl/value :jolt.time/dtf-builder {:pattern (atom "")}))
(defn- b-append! [b s] (swap! (impl/field b :pattern) str s) b)
(__register-class-ctor! "DateTimeFormatterBuilder" (fn [& _] (builder)))
(__register-class-ctor! "java.time.format.DateTimeFormatterBuilder" (fn [& _] (builder)))
(impl/register-type! :jolt.time/dtf-builder
  {:eq (fn [a b] (identical? a b)) :hash (fn [_] 0) :str (fn [_] "DateTimeFormatterBuilder") :cmp nil
   :classes #{"java.time.format.DateTimeFormatterBuilder" "DateTimeFormatterBuilder"}})
(__register-class-methods! :jolt.time/dtf-builder
  ;; [...] optional sections read as always-present: the parse walker defaults
  ;; every field absent at end of input (midnight, zero offset), which is what
  ;; the callers of this builder default to via parseDefaulting anyway.
  {"appendPattern" (fn [b p] (b-append! b (apply str (remove #{\[ \]} (str p)))))
   "appendLiteral" (fn [b s] (b-append! b (str "'" s "'")))
   ;; the X arms of the walkers already print/parse Z for a zero offset and
   ;; ±HH[:MM[:ss]]; colon use follows the requested pattern.
   "appendOffset" (fn [b pattern _no-offset-text]
                    (b-append! b (if (some #{\:} (str pattern)) "XXX" "XX")))
   ;; the parse walker's field map starts at midnight/zero-offset, so absent
   ;; fields already resolve to the defaults every caller installs here.
   "parseDefaulting" (fn [b _field _value] b)
   ;; In the pattern model a fraction is the decimal point (when requested)
   ;; followed by maxWidth 'S' characters. Fixed-width: the JVM trims trailing
   ;; zeros toward minWidth, which a pattern string can't express. Only the
   ;; second-fraction fields map onto 'S'.
   "appendFraction" (fn [b field _min-w max-w point]
                      ;; .name, not str: a ChronoField's toString is its
                      ;; display name (NanoOfSecond) on the JDK
                      (let [fname (if (instance? Enum field) (.name field) (str field))
                            width (case fname
                                    "NANO_OF_SECOND" (min (u/->long max-w) 9)
                                    "MICRO_OF_SECOND" (min (u/->long max-w) 6)
                                    "MILLI_OF_SECOND" (min (u/->long max-w) 3)
                                    (throw (ex-info (str "appendFraction: unsupported field " field)
                                                    {:field fname})))]
                        (b-append! b (str (when point ".") (apply str (repeat width "S"))))))
   "parseCaseInsensitive" (fn [b] b) "parseLenient" (fn [b] b)
   "optionalStart" (fn [b] b) "optionalEnd" (fn [b] b)
   "toFormatter" (fn [b & _] (formatter @(impl/field b :pattern) "en"))})

;; --- pattern-based parse -----------------------------------------------------
;; Walk the pattern and input together: pattern letters consume digits into fields;
;; literals (quoted or plain) must match. Returns {:year :month :day :hour :min
;; :sec :nano :off}. Good enough for tick's parse-* with a custom formatter.
(defn parse-with-pattern [pattern s]
  (let [n (count pattern) sl (count s)]
    (letfn [(letter? [ch] (let [x (int ch)] (or (and (>= x 65) (<= x 90)) (and (>= x 97) (<= x 122)))))
            (digit? [ch] (let [x (int ch)] (and (>= x 48) (<= x 57))))
            (run-len [i c] (loop [j i] (if (and (< j n) (= (nth pattern j) c)) (recur (inc j)) (- j i))))
            (digits [si k] (loop [j si acc 0 got 0]
                             (if (and (< j sl) (>= (int (nth s j)) 48) (<= (int (nth s j)) 57) (< got k))
                               (recur (inc j) (+ (* acc 10) (- (int (nth s j)) 48)) (inc got))
                               [acc j])))
            ;; does the rest of the pattern spell the fraction out with 'S'
            ;; (outside quoted literals)? Then the seconds arm must not consume it.
            (s-ahead? [i] (loop [j i]
                            (cond (>= j n) false
                                  (= (nth pattern j) \') (let [[j2 _] (scan-quote pattern j n "")] (recur j2))
                                  (= (nth pattern j) \S) true
                                  :else (recur (inc j)))))]
      (loop [i 0 si 0 f {:year 0 :month 1 :day 1 :hour 0 :min 0 :sec 0 :nano 0 :off 0}]
        (if (>= i n) f
          (let [c (nth pattern i) k (run-len i c)]
            (cond
              (= c \') (let [[j o] (scan-quote pattern i n "")]  ; the literal to match
                         (recur j (+ si (count o)) f))
              (= c \y) (let [[v si'] (digits si (max k 4))] (recur (+ i k) si' (assoc f :year (if (<= k 2) (+ 2000 v) v))))
              (= c \M) (let [[v si'] (digits si 2)] (recur (+ i k) si' (assoc f :month v)))
              (= c \d) (let [[v si'] (digits si 2)] (recur (+ i k) si' (assoc f :day v)))
              (= c \H) (let [[v si'] (digits si 2)] (recur (+ i k) si' (assoc f :hour v)))
              (= c \m) (let [[v si'] (digits si 2)] (recur (+ i k) si' (assoc f :min v)))
              (= c \s) (let [[v si'] (digits si 2)
                             ;; ISO patterns (…ss XXX) don't spell out the fraction; consume an
                             ;; optional ".fffffffff" so 10:59:13.417Z keeps its millis — but only
                             ;; when the pattern doesn't carry its own 'S' run for it.
                             [nano si2] (if (and (< si' sl) (= \. (nth s si'))
                                             (not (s-ahead? (+ i k))))
                                          (let [[fv fend] (digits (inc si') 9)]
                                            [(* fv (u/pow10 (max 0 (- 9 (dec (- fend si')))))) fend])
                                          [(:nano f) si'])]
                         (recur (+ i k) si2 (assoc f :sec v :nano nano)))
              (= c \S) (let [[v si'] (digits si k)] (recur (+ i k) si' (assoc f :nano (* v (u/pow10 (max 0 (- 9 k)))))))
              ;; VV over a region id ("Europe/Paris") reads the id as the zone
              (and (= c \V) (< si sl) (letter? (nth s si))
                   (not (and (#{\Z \z} (nth s si))
                             (or (= (inc si) sl) (not (letter? (nth s (inc si))))))))
                (let [end (loop [j si] (if (and (< j sl) (let [ch (nth s j)] (or (letter? ch) (digit? ch) (#{\/ \_ \+ \- \~ \.} ch)))) (recur (inc j)) j))]
                  (recur (+ i k) end (assoc f :zone (subs s si end))))
              (or (= c \V) (= c \X) (= c \Z) (= c \z))
                (if (and (< si sl) (#{\Z \z} (nth s si)))
                  (recur (+ i k) (inc si) (assoc f :off 0 :has-off true))
                  (let [end (loop [j si] (if (and (< j sl) (or (#{\+ \- \:} (nth s j)) (and (>= (int (nth s j)) 48) (<= (int (nth s j)) 57)))) (recur (inc j)) j))]
                    (recur (+ i k) end (if (> end si)
                                         (assoc f :off (z/parse-zone-offset (subs s si end)) :has-off true)
                                         f))))
              :else (recur (inc i) (inc si) f))))))))

;; --- strict pattern parse (the TemporalQuery arity of .parse) ----------------
;; parse-with-pattern above is deliberately lenient — tick's custom formatters
;; lean on that. The query arity has to be STRICT instead: malli's -safe decode
;; relies on a malformed string THROWING so the input passes through unchanged.
;; Literals must match, numeric runs must fill their width, an offset letter
;; requires an offset, and the input must be fully consumed. A pattern carrying
;; letters this walker doesn't model (text months, day names, am/pm, zone
;; names) falls back to the lenient walk.
(defn- pat-letter? [c]
  (let [x (int c)] (or (and (>= x 65) (<= x 90)) (and (>= x 97) (<= x 122)))))
(defn- strict-parseable? [pattern]
  (let [n (count pattern)]
    (loop [i 0]
      (if (>= i n)
        true
        (let [c (nth pattern i)]
          (cond
            (= c \') (let [[j _] (scan-quote pattern i n "")] (recur j))
            (pat-letter? c)
            (let [k (loop [j i] (if (and (< j n) (= (nth pattern j) c)) (recur (inc j)) (- j i)))]
              (cond
                (and (= c \M) (> k 2)) false          ; MMM+ is a text month
                (contains? #{\y \M \d \H \m \s \S \X \x \Z \z} c) (recur (+ i k))
                :else false))
            :else (recur (inc i))))))))

(defn- strict-error! [s]
  (throw (jolt.host/throwable "java.time.format.DateTimeParseException"
                              (str "Text '" s "' could not be parsed"))))

(defn- parse-fields-strict [pattern s]
  (let [n (count pattern) sl (count s)]
    (letfn [(run-len [i c] (loop [j i] (if (and (< j n) (= (nth pattern j) c)) (recur (inc j)) (- j i))))
            (digit? [ch] (let [x (int ch)] (and (>= x 48) (<= x 57))))
            (digits-exact [si k]
              (loop [j si acc 0 got 0]
                (if (= got k)
                  [acc j]
                  (if (and (< j sl) (digit? (nth s j)))
                    (recur (inc j) (+ (* acc 10) (- (int (nth s j)) 48)) (inc got))
                    (strict-error! s)))))
            (frac-nanos [si]                     ; after the '.', 1-9 digits -> [nano end]
              (let [end (loop [j si] (if (and (< j sl) (digit? (nth s j)) (< (- j si) 9)) (recur (inc j)) j))]
                (when (= end si) (strict-error! s))
                [(* (reduce (fn [a j] (+ (* a 10) (- (int (nth s j)) 48))) 0 (range si end))
                    (u/pow10 (- 9 (- end si))))
                 end]))
            (at-offset-or-end? [si] (or (>= si sl) (contains? #{\Z \z \+ \-} (nth s si))))]
      (loop [i 0 si 0 f {:year 0 :month 1 :day 1 :hour 0 :min 0 :sec 0 :nano 0 :off 0 :has-off false}]
        (if (>= i n)
          (if (= si sl) f (strict-error! s))     ; unconsumed input
          (let [c (nth pattern i) k (run-len i c)]
            (cond
              (= c \') (let [[j o] (scan-quote pattern i n "")]
                         (if (and (<= (+ si (count o)) sl) (= o (subs s si (+ si (count o)))))
                           (recur j (+ si (count o)) f)
                           (strict-error! s)))
              (= c \y) (let [[v si'] (digits-exact si (if (<= k 2) k 4))]
                          (recur (+ i k) si' (assoc f :year (if (<= k 2) (+ 2000 v) v))))
              (= c \M) (let [[v si'] (digits-exact si k)] (recur (+ i k) si' (assoc f :month v)))
              (= c \d) (let [[v si'] (digits-exact si k)] (recur (+ i k) si' (assoc f :day v)))
              (= c \H) (let [[v si'] (digits-exact si k)] (recur (+ i k) si' (assoc f :hour v)))
              (= c \m) (let [[v si'] (digits-exact si k)] (recur (+ i k) si' (assoc f :min v)))
              (= c \s) (let [[v si'] (digits-exact si k)
                              [nano si2] (if (and (< si' sl) (= \. (nth s si')))
                                           (frac-nanos (inc si'))
                                           [(:nano f) si'])]
                          (recur (+ i k) si2 (assoc f :sec v :nano nano)))
              (= c \S) (let [[v si'] (digits-exact si k)]
                          (recur (+ i k) si' (assoc f :nano (* v (u/pow10 (max 0 (- 9 k)))))))
              (contains? #{\X \x \Z \z} c)
              (cond
                (and (< si sl) (contains? #{\Z \z} (nth s si)))
                (recur (+ i k) (inc si) (assoc f :off 0 :has-off true))
                (and (< si sl) (contains? #{\+ \-} (nth s si)))
                (let [end (loop [j (inc si)] (if (and (< j sl) (or (= \: (nth s j)) (digit? (nth s j)))) (recur (inc j)) j))]
                  (recur (+ i k) end (assoc f :off (z/parse-zone-offset (subs s si end)) :has-off true)))
                :else (strict-error! s))
              ;; unquoted literal: must match — except an omitted ":ss[.fff]"
              ;; tail (ISO optional seconds) when the input sits at its offset
              ;; or its end.
              :else
              (cond
                (and (< si sl) (= (nth s si) c)) (recur (inc i) (inc si) f)
                (and (= c \:) (< (inc i) n) (= \s (nth pattern (inc i))) (at-offset-or-end? si))
                (recur (+ (inc i) (run-len (inc i) \s)) si f)
                :else (strict-error! s)))))))))

;; ISO_ZONED_DATE_TIME input parses as ISO (parse-zoned), gated by a strict
;; shape check the pattern walker can't express (zone brackets, 1-9 fraction
;; digits, optional seconds).
(def ^:private iso-zoned-re
  #"[+-]?\d{4,9}-\d{2}-\d{2}T\d{2}:\d{2}(:\d{2}(\.\d{1,9})?)?([Zz]|[+-]\d{2}(:?\d{2}(:?\d{2})?)?)(\[[A-Za-z0-9~/._+-]+\])?")
(defn- parse-zoned-strict [s]
  (if (re-matches iso-zoned-re s) (zd/parse-zoned s) (strict-error! s)))

;; What a parse yields, as java.time resolves its fields: the instant when an
;; offset was parsed, read in the zone the text names or the override zone
;; (withZone) when there is one, else at that offset; with no offset, the local
;; reading placed in that zone; with neither, the local reading. `lenient?`
;; keeps the TemporalQuery arity's answer for that last case, an OffsetDateTime
;; at zero, which its callers' X/from read as UTC.
(defn- parsed-ldt [f]
  (l/local-dt (days-from-civil (:year f) (:month f) (:day f))
              (u/hmsn->nano (:hour f) (:min f) (:sec f) (:nano f))))
(defn- parsed-utc-nanos [f]
  (let [ldt (parsed-ldt f)]
    (- (+ (* (l/ldt-epoch-day ldt) u/nanos-per-day) (l/ldt-nod ldt)) (* (or (:off f) 0) u/nanos-per-sec))))
(defn- zoned-in [x zone] (zd/zoned-of-instant (instant-nanos x) zone))
(defn- accessor-of [f zone lenient?]
  (let [zone (or (:zone f) zone)]
    (cond
      (:has-off f) (if zone
                     (zd/zoned-of-instant (parsed-utc-nanos f) zone)
                     (let [ldt (parsed-ldt f)] (zd/odt (l/ldt-epoch-day ldt) (l/ldt-nod ldt) (:off f))))
      zone (zd/zoned-of-ldt (parsed-ldt f) zone)
      lenient? (let [ldt (parsed-ldt f)] (zd/odt (l/ldt-epoch-day ldt) (l/ldt-nod ldt) (or (:off f) 0)))
      :else (parsed-ldt f))))
(defn- parsed-value [fmt s]
  (let [zone (impl/field fmt :zone)]
    (if (and (= :zoned (impl/field fmt :iso)) (str/includes? s "["))
      (let [x (zd/parse-zoned s)] (if zone (zoned-in x zone) x))
      (accessor-of (parse-with-pattern (fmt-pattern fmt) s) zone false))))

(defn- fmt-of [f] (if (and (impl/jt? f) (fmt? f)) (fmt-pattern f) (str f)))
(defn- pfields [fmt s] (parse-with-pattern (fmt-of fmt) (str s)))

;; formatter-aware parse, overriding the ISO-only statics for the 2-arg case.
(defn- ldt-of [f] (l/local-dt (days-from-civil (:year f) (:month f) (:day f))
                              (u/hmsn->nano (:hour f) (:min f) (:sec f) (:nano f))))
(statics! ["LocalDate" "java.time.LocalDate"] {"parse" (fn [s & fmt] (if (seq fmt) (let [f (pfields (first fmt) s)] (l/local-date (days-from-civil (:year f) (:month f) (:day f)))) (l/local-date (l/parse-iso-date (str s)))))})
(statics! ["LocalTime" "java.time.LocalTime"] {"parse" (fn [s & fmt] (if (seq fmt) (let [f (pfields (first fmt) s)] (l/local-time (u/hmsn->nano (:hour f) (:min f) (:sec f) (:nano f)))) (l/local-time (u/parse-hms->nano (str s)))))})
(statics! ["LocalDateTime" "java.time.LocalDateTime"] {"parse" (fn [s & fmt] (if (seq fmt) (ldt-of (pfields (first fmt) s)) (let [d (str s) ti (.indexOf d "T")] (l/local-dt (l/parse-iso-date (subs d 0 ti)) (u/parse-hms->nano (subs d (inc ti)))))))})
(statics! ["YearMonth" "java.time.YearMonth"] {"parse" (fn [s & fmt] (if (seq fmt) (let [f (pfields (first fmt) s)] (y/year-month (:year f) (:month f))) (y/year-month (u/digits-at (str s) 0 4) (u/digits-at (str s) 5 2))))})
;; With a formatter: a parsed offset fixes the instant, read in the zone the
;; text names, else the formatter's override zone, else at the offset; with no
;; offset the local reading is placed in that zone (at zero when none is known).
(statics! ["ZonedDateTime" "java.time.ZonedDateTime"]
  {"parse" (fn [s & fmt]
             (if (seq fmt)
               (let [fmt (first fmt)
                     zone (when (and (impl/jt? fmt) (fmt? fmt)) (impl/field fmt :zone))
                     f (pfields fmt s)
                     zone (or (:zone f) zone)]
                 (cond
                   (:has-off f) (zd/zoned-of-instant (parsed-utc-nanos f) (or zone (z/zo-id (:off f))))
                   zone (zd/zoned-of-ldt (parsed-ldt f) zone)
                   :else (zd/zoned-of-instant (parsed-utc-nanos f) (z/zo-id 0))))
               (zd/parse-zoned (str s))))})
(statics! ["OffsetTime" "java.time.OffsetTime"] {"parse" (fn [s & fmt] (if (seq fmt) (let [f (pfields (first fmt) s)] (zd/ot (u/hmsn->nano (:hour f) (:min f) (:sec f) (:nano f)) (:off f))) (zd/parse-offset-time-iso (str s))))})
(statics! ["OffsetDateTime" "java.time.OffsetDateTime"] {"parse" (fn [s & fmt] (if (seq fmt) (let [fmt (first fmt) f (pfields fmt s) zone (when (and (impl/jt? fmt) (fmt? fmt)) (impl/field fmt :zone)) off (if (or (:has-off f) (not zone)) (or (:off f) 0) (impl/field (zd/zoned-of-ldt (parsed-ldt f) zone) :off))] (zd/odt (days-from-civil (:year f) (:month f) (:day f)) (u/hmsn->nano (:hour f) (:min f) (:sec f) (:nano f)) off)) (let [ss (str s) nanos (inst/parse-iso-instant ss) op (some (fn [i] (when (#{\Z \z \+ \-} (nth ss i)) i)) (range (inc (.indexOf ss "T")) (count ss))) off (z/parse-zone-offset (if op (subs ss op) "Z")) local (+ nanos (* off u/nanos-per-sec))] (zd/odt (u/floor-div local u/nanos-per-day) (u/floor-mod local u/nanos-per-day) off))))})
