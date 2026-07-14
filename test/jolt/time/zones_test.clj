(ns jolt.time.zones-test
  "Stage 5a gate: ZoneOffset, ZoneId, ZoneRules + DST offset resolution.
  Run under JOLT_NO_JAVA_TIME=1."
  (:require [clojure.test :refer [deftest is]]
            [jolt.time.zones])
  (:import [java.time ZoneOffset ZoneId Instant]))

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
