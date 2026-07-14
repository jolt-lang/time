(ns jolt.time-test-runner
  "Run tick's suite plus the migrated jolt java.time cases and exit non-zero on
  any failure, error, or namespace that fails to load. jolt.time is required first
  so the java.time host shim is installed before tick loads."
  (:require [clojure.test :as test]
            [jolt.time]))

(def suites
  '[tick.api-test
    tick.internals-test
    tick.addon-libs-test
    tick.alpha.interval-test])

(defn -main [& _]
  (let [loaded (reduce (fn [acc ns-sym]
                         (try (require ns-sym) (conj acc ns-sym)
                              (catch Throwable e
                                (println "LOAD FAIL" ns-sym "—" (ex-message e))
                                acc)))
                       [] suites)
        r (if (seq loaded) (apply test/run-tests loaded) {:test 0 :pass 0 :fail 0 :error 0})
        bad (- (count suites) (count loaded))]
    (println "\n=== time gate:" (:test r) "tests,"
             (:pass r) "pass," (:fail r) "fail," (:error r) "error,"
             bad "suites failed to load ===")
    (flush)
    (System/exit (if (and (zero? (:fail r 0)) (zero? (:error r 0)) (zero? bad)) 0 1))))
