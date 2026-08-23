(ns jolt.time.zones-test
  "Stage 5a gate: ZoneOffset, ZoneId, ZoneRules + DST offset resolution.
  Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.zones])
  (:import [java.time ZoneOffset ZoneId Instant Clock]))

(defn- off-at [zone iso]
  (.getTotalSeconds (.getOffset (.getRules (ZoneId/of zone)) (Instant/parse iso))))

(deftest zone-offset
  (is (= -18000 (.getTotalSeconds (ZoneOffset/ofHours -5))))
  (is (= "+05:30" (.getId (ZoneOffset/of "+05:30"))))
  (is (= "Z" (str ZoneOffset/UTC)))
  (is (= 19800 (.getTotalSeconds (ZoneOffset/ofHoursMinutes 5 30))))
  (is (= "-08:00" (str (ZoneOffset/ofHours -8)))))

(deftest zone-id
  (is (= "America/New_York" (.getId (ZoneId/of "America/New_York"))))
  (is (= "Europe/Paris" (.getId (ZoneId/of "Europe/Paris"))))
  (is (= (ZoneId/of "America/New_York") (ZoneId/of "America/New_York"))))

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
