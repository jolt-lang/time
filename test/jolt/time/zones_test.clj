(ns jolt.time.zones-test
  "Stage 5a gate: ZoneOffset, ZoneId, ZoneRules + DST offset resolution.
  Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.zones])
  (:import [java.time ZoneOffset ZoneId Instant Clock]
           [java.util TimeZone]))

(defn- off-at [zone iso]
  (.getTotalSeconds (.getOffset (.getRules (ZoneId/of zone)) (Instant/parse iso))))

(deftest zone-offset
  (is (= -18000 (.getTotalSeconds (ZoneOffset/ofHours -5))))
  (is (= "+05:30" (.getId (ZoneOffset/of "+05:30"))))
  (is (= "Z" (str ZoneOffset/UTC)))
  (is (= 19800 (.getTotalSeconds (ZoneOffset/ofHoursMinutes 5 30))))
  (is (= "-08:00" (str (ZoneOffset/ofHours -8)))))

(deftest zone-offset-validates-java-component-contract
  (doseq [f [#(ZoneOffset/ofHours 19)
             #(ZoneOffset/ofHours -19)
             #(ZoneOffset/ofHoursMinutes 1 60)
             #(ZoneOffset/ofHoursMinutes 1 -30)
             #(ZoneOffset/ofHoursMinutes -1 30)
             #(ZoneOffset/ofHoursMinutesSeconds 0 30 -1)
             #(ZoneOffset/ofHoursMinutesSeconds 0 -30 1)
             #(ZoneOffset/ofHoursMinutesSeconds 18 0 1)
             #(ZoneOffset/ofTotalSeconds 64801)
             #(ZoneOffset/of "+01:99")
             #(ZoneOffset/of "+18:00:01")]]
    (is (thrown? java.time.DateTimeException (f))))
  (is (= [64800 -64800 1800 -1800 -1]
         (mapv #(.getTotalSeconds %)
               [(ZoneOffset/ofHours 18)
                (ZoneOffset/ofHours -18)
                (ZoneOffset/ofHoursMinutes 0 30)
                (ZoneOffset/ofHoursMinutes 0 -30)
                (ZoneOffset/ofHoursMinutesSeconds 0 0 -1)])))
  (is (= "Zone offset minutes and seconds must be positive because hours is positive"
         (try
           (ZoneOffset/ofHoursMinutes 1 -60)
           nil
           (catch java.time.DateTimeException e
             (ex-message e))))))

(deftest zone-id
  (is (= "America/New_York" (.getId (ZoneId/of "America/New_York"))))
  (is (= "Europe/Paris" (.getId (ZoneId/of "Europe/Paris"))))
  (is (= (ZoneId/of "America/New_York") (ZoneId/of "America/New_York"))))

;; A region id the tz database doesn't know throws ZoneRulesException, a
;; DateTimeException, as on the JVM. It used to come back as a zone with UTC rules.
(deftest zone-id-of-rejects-unknown-regions
  (doseq [id ["Nope/Zone" "garbage" "Nope" "australia/sydney" "PST" "system"]]
    (is (= ["java.time.zone.ZoneRulesException" (str "Unknown time-zone ID: " id)]
           (try (ZoneId/of id) nil
                (catch Throwable e [(.getName (class e)) (ex-message e)])))
        id))
  (is (= :dte (try (ZoneId/of "Nope/Zone") :no-throw (catch java.time.DateTimeException _ :dte))))
  (doseq [id ["Australia/Sydney" "UTC" "GMT" "UT" "Etc/UTC" "Z" "+11:00" "UTC+11" "GMT+1100"
              "US/Pacific" "Asia/Calcutta" "Europe/Kyiv" "Etc/GMT+5" "America/Argentina/Buenos_Aires"]]
    (is (some? (ZoneId/of id)) id))
  ;; the two-arg form expands through the alias map first
  (is (= "America/Los_Angeles" (.getId (ZoneId/of "PST" ZoneId/SHORT_IDS))))
  (is (= "Australia/Sydney" (.getId (ZoneId/of "Australia/Sydney" ZoneId/SHORT_IDS))))
  (is (= "java.time.zone.ZoneRulesException"
         (try (ZoneId/of "Nope/Zone" ZoneId/SHORT_IDS) nil (catch Throwable e (.getName (class e)))))))

;; A region id with no slash is still a tz region (CET, NZ, Japan), and a
;; UTC/GMT/UT prefix with an offset is fixed at that offset. Both used to read
;; as UTC. Expected values are the JDK's.
(deftest zone-offsets-of-unslashed-and-prefixed-ids
  (doseq [[id jan jul] [["CET" 3600 7200] ["EST5EDT" -18000 -14400] ["NZ" 46800 43200]
                        ["Japan" 32400 32400] ["UTC+01:00" 3600 3600] ["GMT-05:30" -19800 -19800]
                        ["UT+11" 39600 39600] ["GMT0" 0 0] ["US/Pacific" -28800 -25200]]]
    (is (= [jan jul] [(off-at id "2020-01-15T12:00:00Z") (off-at id "2020-07-15T12:00:00Z")]) id))
  (is (= ["UTC+01:00" "GMT-05:30" "UT+11:00" "GMT" "UTC"]
         (map #(.getId (ZoneId/of %)) ["UTC+01:00" "GMT-0530" "UT+11" "GMT+0" "UTC-00:00"])))
  (is (= "2020-07-01T02:00+02:00[CET]"
         (str (java.time.ZonedDateTime/ofInstant (Instant/parse "2020-07-01T00:00:00Z") (ZoneId/of "CET"))))))

;; An offset id gives a ZoneOffset, as on the JVM, so (instance? ZoneOffset z)
;; tells a fixed offset from a named zone. A zone whose rules are fixed
;; normalizes to its ZoneOffset; one with transitions stays itself.
(deftest zone-id-of-offset-ids-are-zone-offsets
  (doseq [[id oid] [["+10:00" "+10:00"] ["Z" "Z"] ["-03:30" "-03:30"] ["+10" "+10:00"]]]
    (let [z (ZoneId/of id)]
      (is (instance? ZoneOffset z) id)
      (is (instance? ZoneId z) id)
      (is (= oid (.getId z)) id)
      (is (= (ZoneOffset/of id) z) id)))
  (doseq [id ["UTC+10" "Australia/Sydney" "UTC" "CET"]]
    (is (not (instance? ZoneOffset (ZoneId/of id))) id))
  (doseq [[id oid] [["+10:00" "+10:00"] ["Z" "Z"] ["UTC+10" "+10:00"] ["GMT-05:30" "-05:30"]
                    ["UT+11" "+11:00"] ["UTC" "Z"] ["GMT" "Z"] ["Etc/UTC" "Z"] ["GMT0" "Z"]
                    ["Etc/GMT+5" "-05:00"]]]
    (let [n (.normalized (ZoneId/of id))]
      (is (instance? ZoneOffset n) id)
      (is (= oid (.getId n)) id)))
  (doseq [id ["Australia/Sydney" "CET" "NZ" "US/Pacific"]]
    (is (= id (.getId (.normalized (ZoneId/of id)))) id))
  (doseq [[id fixed?] [["UTC+10" true] ["UTC" true] ["Etc/GMT+5" true] ["+02:00" true]
                       ["CET" false] ["NZ" false] ["Australia/Sydney" false]]]
    (is (= fixed? (.isFixedOffset (.getRules (ZoneId/of id)))) id))
  (is (= "2020-01-01T00:00+10:00"
         (str (java.time.ZonedDateTime/ofInstant (Instant/parse "2019-12-31T14:00:00Z") (ZoneId/of "+10:00"))))))

(deftest dst-offsets
  ;; America/New_York: EST (-5) in January, EDT (-4) in July
  (is (= -18000 (off-at "America/New_York" "2020-01-15T12:00:00Z")))
  (is (= -14400 (off-at "America/New_York" "2020-07-15T12:00:00Z")))
  ;; Europe/Paris: CET (+1) in January, CEST (+2) in July
  (is (= 3600 (off-at "Europe/Paris" "2020-01-15T12:00:00Z")))
  (is (= 7200 (off-at "Europe/Paris" "2020-07-15T12:00:00Z")))
  ;; fixed / UTC
  (is (= 0 (off-at "UTC" "2020-07-15T12:00:00Z"))))

;; --- the machine's own zone ---------------------------------------------------
;; systemDefault used to answer "Z" unconditionally. What it answers now depends
;; on the machine, so these pin the decisions rather than a particular zone: how
;; a candidate string is classified, how a tzdata path is parsed, and that the
;; two entry points agree.

(def ^:private as-zone-id #'jolt.time.zones/as-zone-id)
(def ^:private zoneinfo-name #'jolt.time.zones/zoneinfo-name)

(deftest zone-id-classification
  (is (= "America/New_York" (as-zone-id "America/New_York")))
  (is (= "America/Argentina/Buenos_Aires" (as-zone-id "America/Argentina/Buenos_Aires")))
  (is (= "UTC" (as-zone-id "UTC")))
  (is (= "+05:30" (as-zone-id "+05:30")))
  ;; SHORT_IDS expand, so a bare abbreviation never reaches resolve-zone, which
  ;; reads one as a 0-offset stub and would silently place the machine on UTC
  (is (= "-05:00" (as-zone-id "EST")))
  (is (= "Australia/Sydney" (as-zone-id "AET")))
  ;; a POSIX rule string is legal in TZ and is not a zone id. Note it contains a
  ;; slash, so testing for one would accept it.
  (is (nil? (as-zone-id "AEST-10AEDT,M10.1.0,M4.1.0/3")))
  (is (nil? (as-zone-id "EST5EDT,M3.2.0,M11.1.0")))
  (is (nil? (as-zone-id "Not/AZone")))
  (is (nil? (as-zone-id "")))
  (is (nil? (as-zone-id nil))))

(deftest zoneinfo-path-parse
  (is (= "Australia/Sydney" (zoneinfo-name "/usr/share/zoneinfo/Australia/Sydney")))
  ;; macOS interposes a version directory, so the split is on the LAST marker
  (is (= "Australia/Sydney"
         (zoneinfo-name "/private/var/db/timezone/tz/2026c.1.0/zoneinfo/Australia/Sydney")))
  (is (= "America/Argentina/Buenos_Aires"
         (zoneinfo-name "/usr/share/zoneinfo/America/Argentina/Buenos_Aires")))
  (is (nil? (zoneinfo-name "/etc/localtime")))
  (is (nil? (zoneinfo-name "/usr/share/zoneinfo/")))
  (is (nil? (zoneinfo-name nil))))

(deftest system-default-zone
  (let [id (jolt.time.zones/system-zone-id)]
    (is (string? id))
    (is (seq id))
    ;; compared as ZoneIds, not id strings: the UTC/GMT aliases normalize to "Z",
    ;; so a string round-trip would fail on a machine running under TZ=UTC
    (is (= (ZoneId/of id) (ZoneId/systemDefault)))
    ;; these were two separate hardcoded "Z" literals and could drift apart
    (is (= (ZoneId/systemDefault) (.getZone (Clock/systemDefaultZone))))))

;; ZoneId/systemDefault is TimeZone.getDefault().toZoneId() on the JVM, so
;; TimeZone/setDefault moves it, and every `now` with it. Skipped on a jolt
;; without TimeZone/setDefault (before 0.8.24).
(deftest system-default-follows-timezone-set-default
  (when (try (TimeZone/setDefault (TimeZone/getTimeZone "Asia/Kolkata")) true
             (catch Throwable _ false))
    (try
      (is (= (ZoneId/of "Asia/Kolkata") (ZoneId/systemDefault)))
      (is (= (ZoneId/of "Asia/Kolkata") (.getZone (Clock/systemDefaultZone))))
      (is (= (ZoneId/of "Asia/Kolkata") (.getZone (java.time.ZonedDateTime/now))))
      (TimeZone/setDefault (TimeZone/getTimeZone "America/Toronto"))
      (is (= (ZoneId/of "America/Toronto") (ZoneId/systemDefault)))
      (finally (TimeZone/setDefault nil))))
  ;; reset: back to the system's zone
  (is (= (ZoneId/of (.getID (TimeZone/getDefault))) (ZoneId/systemDefault))))

;; --- core's default zone --------------------------------------------------------
;; Loading this library registers system-zone-id as core's default-zone
;; provider. A jolt before 0.8.24 answers TZ, else that provider, else UTC; from
;; 0.8.24 core reads the machine's zone itself and the provider is only its
;; fallback where the system names none. Pinned through with-redefs, so on an
;; older core TimeZone/getDefault is the provider's answer and on a newer one
;; the machine's. Core consults neither under TZ, so the check is skipped there.
(deftest core-default-zone-is-this-library's
  (when (nil? (System/getenv "TZ"))
    (let [machine (let [z (jolt.time.zones/system-zone-id)] (if (= z "Z") "UTC" z))]
      (with-redefs [jolt.time.zones/system-zone-id (constantly "Asia/Tokyo")]
        (is (contains? #{"Asia/Tokyo" machine} (.getID (TimeZone/getDefault))))))
    ;; and with the real lookup, the two entry points name one zone
    (is (= (ZoneId/systemDefault) (ZoneId/of (.getID (TimeZone/getDefault)))))))
