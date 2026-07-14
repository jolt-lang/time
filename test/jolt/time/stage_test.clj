(ns jolt.time.stage-test
  "Runs the per-stage unit gates for the java.time port. Run under
  JOLT_NO_JAVA_TIME=1 so the library is the sole java.time provider."
  (:require [clojure.test :as test]
            [jolt.time.impl-test]
            [jolt.time.enums-test]
            [jolt.time.local-test]
            [jolt.time.amount-test]))

(def suites
  '[jolt.time.impl-test
    jolt.time.enums-test
    jolt.time.local-test
    jolt.time.amount-test])

(defn -main [& _]
  (let [r (apply test/run-tests suites)]
    (System/exit (if (and (zero? (:fail r)) (zero? (:error r))) 0 1))))
