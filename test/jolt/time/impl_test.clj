(ns jolt.time.impl-test
  "Stage 1 gate: the value machinery + civil kernel. Registers a sample type and
  checks that =, hash, str, pr, compare, and instance? all work through jolt's
  value-semantics seams. Run under JOLT_NO_JAVA_TIME=1 so the library is the sole
  java.time provider."
  (:require [clojure.test :refer [deftest is run-tests]]
            [jolt.time.impl :as impl]))

(impl/register-type! :jolt.time/test-date
  {:eq   (fn [a b] (= (impl/field a :epoch-day) (impl/field b :epoch-day)))
   :hash (fn [v] (hash (impl/field v :epoch-day)))
   :str  (fn [v] (let [[y m d] (impl/civil-from-days (impl/field v :epoch-day))]
                   (format "%04d-%02d-%02d" y m d)))
   :cmp  (fn [a b] (compare (impl/field a :epoch-day) (impl/field b :epoch-day)))
   :classes #{"java.time.LocalDate" "LocalDate"
              "java.time.temporal.Temporal" "Temporal"
              "java.lang.Comparable" "Comparable"}})

(defn- mk [y m d] (impl/value :jolt.time/test-date {:epoch-day (impl/days-from-civil y m d)}))

(deftest civil-kernel
  (is (= [2020 1 15] (impl/civil-from-days (impl/days-from-civil 2020 1 15))))
  (is (= [1969 12 31] (impl/civil-from-days (impl/days-from-civil 1969 12 31))))
  (is (= 0 (impl/days-from-civil 1970 1 1)))
  (is (= [2000 2 29] (impl/civil-from-days (impl/days-from-civil 2000 2 29)))))

(deftest opaque
  (let [d (mk 2020 1 15)]
    (is (impl/jt? d))
    (is (not (map? d)))
    (is (not (coll? d)))))

(deftest equality-and-hash
  (let [a (mk 2020 1 15) b (mk 2020 1 15) c (mk 2020 1 16)]
    (is (= a b))
    (is (not= a c))
    (is (= (hash a) (hash b)))
    ;; usable as map/set keys
    (is (= 1 (count (set [a b]))))
    (is (= :hit (get {a :hit} b)))))

(deftest rendering
  (let [d (mk 2020 1 15)]
    (is (= "2020-01-15" (str d)))
    (is (= "2020-01-15" (pr-str d)))))

(deftest ordering
  (let [a (mk 2020 1 15) c (mk 2020 1 16)]
    (is (neg? (compare a c)))
    (is (pos? (compare c a)))
    (is (zero? (compare a (mk 2020 1 15))))
    (is (= [a c] (sort [c a])))))

(deftest instance-checks
  (let [d (mk 2020 1 15)]
    (is (instance? java.time.LocalDate d))
    (is (instance? java.time.temporal.Temporal d))
    (is (instance? java.lang.Comparable d))
    (is (not (instance? java.time.Instant d)))
    (is (not (instance? java.time.LocalDate "nope")))))

(defn -main [& _]
  (let [r (run-tests 'jolt.time.impl-test)]
    (System/exit (if (and (zero? (:fail r)) (zero? (:error r))) 0 1))))
