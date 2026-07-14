(ns jolt.time.zones
  "ZoneOffset, ZoneId, and ZoneRules, plus offset resolution. A named IANA zone's
  offset comes from the core jolt.host/tz-offset-seconds primitive (libc); when
  that is unavailable it degrades to a built-in DST rule table (US/EU/AU/NZ)."
  (:require [jolt.host :as host]
            [clojure.string :as str]
            [jolt.time.impl :as impl :refer [civil-from-days days-from-civil]]
            [jolt.time.util :as u]
            [jolt.time.instant :as inst]
            [jolt.time.local :as l]))

(defn- statics! [names members] (doseq [n names] (__register-class-statics! n members)))
(def ^:private nps u/nanos-per-sec)

;; --- ZoneOffset --------------------------------------------------------------
(defn zone-offset [secs] (impl/value :jolt.time/zone-offset {:secs secs}))
(defn zo-secs [z] (impl/field z :secs))
(defn zo? [z] (= :jolt.time/zone-offset (impl/type-of z)))

(defn zo-id [secs]
  (if (zero? secs) "Z"
    (let [neg (neg? secs) a (abs secs) h (quot a 3600) m (quot (mod a 3600) 60) s (mod a 60)]
      (str (if neg "-" "+") (u/pad2 h) ":" (u/pad2 m) (if (zero? s) "" (str ":" (u/pad2 s)))))))

(defn parse-zone-offset [s]
  (let [s (str s)]
    (if (contains? #{"Z" "z" "UTC" "GMT" "+00:00"} s) 0
      (let [sign (if (= \- (nth s 0)) -1 1)
            body (if (#{\+ \-} (nth s 0)) (subs s 1) s)
            parts (str/split body #":")]
        (if (and (= 1 (count parts)) (> (count (first parts)) 2))
          (let [b (first parts)]
            (* sign (+ (* (or (parse-long (subs b 0 2)) 0) 3600)
                       (* (if (>= (count b) 4) (or (parse-long (subs b 2 4)) 0) 0) 60)
                       (if (>= (count b) 6) (or (parse-long (subs b 4 6)) 0) 0))))
          (* sign (+ (* (or (parse-long (nth parts 0)) 0) 3600)
                     (* (if (> (count parts) 1) (or (parse-long (nth parts 1)) 0) 0) 60)
                     (if (> (count parts) 2) (or (parse-long (nth parts 2)) 0) 0))))))))

(declare odt-offset zdt-offset)
(impl/register-type! :jolt.time/zone-offset
  {:eq (fn [a b] (= (zo-secs a) (zo-secs b))) :hash zo-secs :str (fn [z] (zo-id (zo-secs z)))
   :cmp (fn [a b] (compare (zo-secs a) (zo-secs b)))
   :classes #{"java.time.ZoneOffset" "ZoneOffset" "java.time.ZoneId" "ZoneId"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor" "java.lang.Comparable" "Comparable"}})

(declare zone-rules)
(__register-class-methods! :jolt.time/zone-offset
  {"getId" (fn [z] (zo-id (zo-secs z))) "getTotalSeconds" zo-secs
   "getRules" (fn [z] (zone-rules (zo-id (zo-secs z)) (zo-secs z)))
   "normalized" (fn [z] z)
   "compareTo" (fn [z o] (compare (zo-secs z) (zo-secs o)))
   "equals" (fn [z o] (boolean (and (impl/jt? o) (zo? o) (= (zo-secs z) (zo-secs o)))))
   "hashCode" zo-secs "toString" (fn [z] (zo-id (zo-secs z)))})

(statics! ["ZoneOffset" "java.time.ZoneOffset"]
  {"of" (fn [s] (zone-offset (parse-zone-offset s)))
   "ofTotalSeconds" (fn [n] (zone-offset (u/->long n)))
   "ofHours" (fn [h] (zone-offset (* (u/->long h) 3600)))
   "ofHoursMinutes" (fn [h m] (zone-offset (+ (* (u/->long h) 3600) (* (u/->long m) 60))))
   "ofHoursMinutesSeconds" (fn [h m s] (zone-offset (+ (* (u/->long h) 3600) (* (u/->long m) 60) (u/->long s))))
   "UTC" (zone-offset 0) "MIN" (zone-offset (* -18 3600)) "MAX" (zone-offset (* 18 3600))})

;; --- offset resolution (pure fallback + core libc primitive) -----------------
(def ^:private zone-offset-table
  {"UTC" 0 "GMT" 0 "Z" 0 "Etc/UTC" 0 "Etc/GMT" 0
   "America/New_York" -18000 "America/Chicago" -21600 "America/Denver" -25200
   "America/Los_Angeles" -28800 "America/Toronto" -18000 "America/Sao_Paulo" -10800
   "America/Mexico_City" -21600 "America/Argentina/Buenos_Aires" -10800
   "Europe/London" 0 "Europe/Paris" 3600 "Europe/Berlin" 3600 "Europe/Madrid" 3600
   "Europe/Rome" 3600 "Europe/Amsterdam" 3600 "Europe/Stockholm" 3600 "Europe/Zurich" 3600
   "Europe/Moscow" 10800 "Asia/Tokyo" 32400 "Asia/Shanghai" 28800 "Asia/Kolkata" 19800
   "Asia/Singapore" 28800 "Asia/Dubai" 14400 "Asia/Hong_Kong" 28800
   "Australia/Sydney" 36000 "Australia/Melbourne" 36000 "Australia/Brisbane" 36000
   "Australia/Perth" 28800 "Australia/Adelaide" 34200 "Pacific/Auckland" 43200
   "Pacific/Fiji" 43200 "Africa/Johannesburg" 7200 "Africa/Cairo" 7200 "Africa/Lagos" 3600})

;; id -> [standard-offset rule-family]
(def ^:private dst-zone-table
  {"America/New_York" [-18000 :us] "America/Toronto" [-18000 :us]
   "America/Chicago" [-21600 :us] "America/Denver" [-25200 :us] "America/Los_Angeles" [-28800 :us]
   "Europe/London" [0 :eu] "Europe/Paris" [3600 :eu] "Europe/Berlin" [3600 :eu] "Europe/Madrid" [3600 :eu]
   "Europe/Rome" [3600 :eu] "Europe/Amsterdam" [3600 :eu] "Europe/Stockholm" [3600 :eu] "Europe/Zurich" [3600 :eu]
   "Australia/Sydney" [36000 :au] "Australia/Melbourne" [36000 :au] "Australia/Adelaide" [34200 :au]
   "Pacific/Auckland" [43200 :nz]})

(def ^:private short-ids
  {"ACT" "Australia/Darwin" "AET" "Australia/Sydney" "AGT" "America/Argentina/Buenos_Aires"
   "ART" "Africa/Cairo" "AST" "America/Anchorage" "BET" "America/Sao_Paulo" "BST" "Asia/Dhaka"
   "CAT" "Africa/Harare" "CNT" "America/St_Johns" "CST" "America/Chicago" "CTT" "Asia/Shanghai"
   "EAT" "Africa/Addis_Ababa" "ECT" "Europe/Paris" "IET" "America/Indiana/Indianapolis"
   "IST" "Asia/Kolkata" "JST" "Asia/Tokyo" "MIT" "Pacific/Apia" "NET" "Asia/Yerevan"
   "NST" "Pacific/Auckland" "PLT" "Asia/Karachi" "PNT" "America/Phoenix" "PRT" "America/Puerto_Rico"
   "PST" "America/Los_Angeles" "SST" "Pacific/Guadalcanal" "VST" "Asia/Ho_Chi_Minh"
   "EST" "-05:00" "MST" "-07:00" "HST" "-10:00"})

(def ^:private dst-saving 3600)
(defn- epoch-day-dow [ed] (mod (+ ed 4) 7))
(defn- nth-dow-epoch-day [year month dow n]
  (if (pos? n)
    (let [first-ed (days-from-civil year month 1) shift (mod (- dow (epoch-day-dow first-ed)) 7)]
      (+ first-ed shift (* (dec n) 7)))
    (let [last-ed (dec (days-from-civil year (inc month) 1)) shift (mod (- (epoch-day-dow last-ed) dow) 7)]
      (- last-ed shift (* (dec (- n)) 7)))))
(defn- secs->year [secs] (first (civil-from-days (u/floor-div secs 86400))))

(defn- dst-offset [std rule secs]
  (let [year (secs->year secs)]
    (case rule
      :us (let [spring (- (+ (* (nth-dow-epoch-day year 3 0 2) 86400) (* 2 3600)) std)
                fall   (- (+ (* (nth-dow-epoch-day year 11 0 1) 86400) (* 2 3600)) (+ std dst-saving))]
            (if (and (<= spring secs) (< secs fall)) (+ std dst-saving) std))
      :eu (let [spring (+ (* (nth-dow-epoch-day year 3 0 -1) 86400) 3600)
                fall   (+ (* (nth-dow-epoch-day year 10 0 -1) 86400) 3600)]
            (if (and (<= spring secs) (< secs fall)) (+ std dst-saving) std))
      :au (let [spring (- (+ (* (nth-dow-epoch-day year 10 0 1) 86400) (* 2 3600)) std)
                fall   (- (+ (* (nth-dow-epoch-day year 4 0 1) 86400) (* 3 3600)) (+ std dst-saving))]
            (if (or (>= secs spring) (< secs fall)) (+ std dst-saving) std))
      :nz (let [spring (- (+ (* (nth-dow-epoch-day year 9 0 -1) 86400) (* 2 3600)) std)
                fall   (- (+ (* (nth-dow-epoch-day year 4 0 1) 86400) (* 3 3600)) (+ std dst-saving))]
            (if (or (>= secs spring) (< secs fall)) (+ std dst-saving) std))
      std)))

(defn- fixed-offset-zone? [id] (and (pos? (count id)) (#{\+ \-} (nth id 0))))
(defn- slash? [id] (boolean (some #(= \/ %) id)))

(defn zone-offset-at-instant [id std secs]
  (cond
    (fixed-offset-zone? id) (parse-zone-offset id)
    (slash? id) (let [libc (host/tz-offset-seconds id secs)]
                  (if (some? libc) libc
                    (if-let [[std2 rule] (dst-zone-table id)]
                      (dst-offset std2 rule secs)
                      (get zone-offset-table id std))))
    :else std))
(defn zone-offset-at-local [id std lsecs]
  (zone-offset-at-instant id std (- lsecs (zone-offset-at-instant id std lsecs))))

;; --- ZoneId ------------------------------------------------------------------
(defn zone-id [id off] (impl/value :jolt.time/zone-id {:id id :off off}))
(defn zid-id [z] (impl/field z :id))
(defn zid-off [z] (impl/field z :off))
(defn zid? [z] (= :jolt.time/zone-id (impl/type-of z)))

(defn resolve-zone
  "Any zone designator (string / ZoneId / ZoneOffset) -> [id offset]."
  [z]
  (cond
    (and (impl/jt? z) (zo? z)) [(zo-id (zo-secs z)) (zo-secs z)]
    (and (impl/jt? z) (zid? z)) [(zid-id z) (zid-off z)]
    :else (let [id (str z)]
            (cond
              (contains? #{"Z" "UTC" "GMT" "Etc/UTC" "Etc/GMT" "system"} id) ["Z" 0]
              (fixed-offset-zone? id) (let [s (parse-zone-offset id)] [(zo-id s) s])
              :else (let [rid (get short-ids id id)]
                      (if (slash? rid) [rid (or (host/tz-offset-seconds rid 0) (get zone-offset-table rid 0))]
                          [id 0]))))))
(defn zone-id-of [z] (let [[id off] (resolve-zone z)] (zone-id id off)))

(impl/register-type! :jolt.time/zone-id
  {:eq (fn [a b] (= (zid-id a) (zid-id b))) :hash (fn [z] (hash (zid-id z))) :str zid-id :cmp nil
   :classes #{"java.time.ZoneId" "ZoneId" "java.io.Serializable" "Serializable"}})

(__register-class-methods! :jolt.time/zone-id
  {"getId" zid-id
   "getRules" (fn [z] (zone-rules (zid-id z) (zid-off z)))
   "normalized" (fn [z] (if (and (pos? (count (zid-id z))) (#{\+ \- \Z} (nth (zid-id z) 0))) (zone-offset (zid-off z)) z))
   "getDisplayName" (fn [z & _] (zid-id z))
   "equals" (fn [z o] (boolean (and (impl/jt? o) (zid? o) (= (zid-id z) (zid-id o)))))
   "hashCode" (fn [z] (hash (zid-id z))) "toString" zid-id})

(statics! ["ZoneId" "java.time.ZoneId"]
  {"of" (fn [id & _] (zone-id-of id))
   "systemDefault" (fn [] (zone-id "Z" 0))
   "getAvailableZoneIds" (fn [] (set (keys short-ids)))
   "SHORT_IDS" short-ids})

;; --- ZoneRules ---------------------------------------------------------------
(defn zone-rules [id std] (impl/value :jolt.time/zone-rules {:id id :std std}))
(defn- zr-id [r] (impl/field r :id))
(defn- zr-std [r] (impl/field r :std))

(impl/register-type! :jolt.time/zone-rules
  {:eq (fn [a b] (= (zr-id a) (zr-id b))) :hash (fn [r] (hash (zr-id r))) :str (fn [r] (str "ZoneRules[" (zr-id r) "]")) :cmp nil
   :classes #{"java.time.zone.ZoneRules" "ZoneRules"}})

(__register-class-methods! :jolt.time/zone-rules
  {"getOffset" (fn [r & args]
                 (if (empty? args) (zone-offset (zr-std r))
                   (let [a (first args)]
                     (cond
                       (and (impl/jt? a) (inst/inst? a))
                       (zone-offset (zone-offset-at-instant (zr-id r) (zr-std r) (u/floor-div (inst/inst-nanos a) nps)))
                       (and (impl/jt? a) (= :jolt.time/local-date-time (impl/type-of a)))
                       (zone-offset (zone-offset-at-local (zr-id r) (zr-std r)
                                     (+ (* (l/ldt-epoch-day a) 86400) (u/floor-div (l/ldt-nod a) nps))))
                       :else (zone-offset (zr-std r))))))
   "isFixedOffset" (fn [r] (not (slash? (zr-id r))))
   "toString" (fn [r] (str "ZoneRules[" (zr-id r) "]"))})
