(ns jolt.time.year
  "Year and YearMonth."
  (:require [jolt.time.impl :as impl :refer [civil-from-days days-from-civil]]
            [jolt.time.util :as u]
            [jolt.time.enums :as e]
            [jolt.time.local :as l]))

(defn- statics! [names members]
  (doseq [n names] (__register-class-statics! n members)))

;; --- Year --------------------------------------------------------------------

(defn year [y] (impl/value :jolt.time/year {:value y}))
(defn year-val [y] (impl/field y :value))
(defn year? [y] (= :jolt.time/year (impl/type-of y)))

(impl/register-type! :jolt.time/year
  {:eq   (fn [a b] (= (year-val a) (year-val b)))
   :hash (fn [y] (year-val y))
   :str  (fn [y] (str (year-val y)))
   :cmp  (fn [a b] (compare (year-val a) (year-val b)))
   :classes #{"java.time.Year" "Year"
              "java.time.temporal.Temporal" "Temporal"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable"}})

(declare year-month)

(__register-class-methods! :jolt.time/year
  {"getValue"   year-val
   "isLeap"     (fn [y] (u/leap? (year-val y)))
   "length"     (fn [y] (if (u/leap? (year-val y)) 366 365))
   "plusYears"  (fn [y n] (year (+ (year-val y) (u/->long n))))
   "minusYears" (fn [y n] (year (- (year-val y) (u/->long n))))
   "atMonth"    (fn [y m] (year-month (year-val y) (u/->long m)))
   "atDay"      (fn [y doy] (l/local-date (+ (days-from-civil (year-val y) 1 1) (dec (u/->long doy)))))
   "isBefore"   (fn [y o] (< (year-val y) (year-val o)))
   "isAfter"    (fn [y o] (> (year-val y) (year-val o)))
   "compareTo"  (fn [y o] (compare (year-val y) (year-val o)))
   "equals"     (fn [y o] (boolean (and (impl/jt? o) (year? o) (= (year-val y) (year-val o)))))
   "hashCode"   year-val
   "toString"   (fn [y] (str (year-val y)))})

(statics! ["Year" "java.time.Year"]
  {"of"    (fn [y] (year (u/->long y)))
   "now"   (fn [& _] (year (first (civil-from-days (u/floor-div (System/currentTimeMillis) 86400000)))))
   "isLeap" (fn [y] (u/leap? (u/->long y)))
   "parse" (fn [s & _] (year (or (parse-long (str s)) (throw (ex-info "could not parse Year" {})))))
   "MIN_VALUE" -999999999
   "MAX_VALUE" 999999999})

;; --- YearMonth ---------------------------------------------------------------

(defn year-month
  "Construct a normalized YearMonth (month overflow rolls into the year)."
  [y m]
  (let [ym (+ (* y 12) (dec m))]
    (impl/value :jolt.time/year-month {:year (u/floor-div ym 12) :month (inc (u/floor-mod ym 12))})))
(defn ym-year [x] (impl/field x :year))
(defn ym-month [x] (impl/field x :month))
(defn ym? [x] (= :jolt.time/year-month (impl/type-of x)))
(defn- ym-key [x] [(ym-year x) (ym-month x)])
(defn- ym-cmp [a b] (compare (ym-key a) (ym-key b)))
(defn- ym-plus-months [x n]
  (let [tm (+ (* (ym-year x) 12) (dec (ym-month x)) n)]
    (impl/value :jolt.time/year-month {:year (u/floor-div tm 12) :month (inc (u/floor-mod tm 12))})))
(defn- ym->string [x] (str (u/pad4 (ym-year x)) "-" (u/pad2 (ym-month x))))

(impl/register-type! :jolt.time/year-month
  {:eq   (fn [a b] (= (ym-key a) (ym-key b)))
   :hash (fn [x] (+ (* (ym-year x) 13) (ym-month x)))
   :str  ym->string
   :cmp  ym-cmp
   :classes #{"java.time.YearMonth" "YearMonth"
              "java.time.temporal.Temporal" "Temporal"
              "java.time.temporal.TemporalAccessor" "TemporalAccessor"
              "java.time.temporal.TemporalAdjuster" "TemporalAdjuster"
              "java.lang.Comparable" "Comparable"}})

(__register-class-methods! :jolt.time/year-month
  {"getYear"       ym-year
   "getMonthValue" ym-month
   "getMonth"      (fn [x] (e/month (ym-month x)))
   "lengthOfMonth" (fn [x] (u/len-of-month (ym-year x) (ym-month x)))
   "lengthOfYear"  (fn [x] (if (u/leap? (ym-year x)) 366 365))
   "isLeapYear"    (fn [x] (u/leap? (ym-year x)))
   "plusMonths"    (fn [x n] (ym-plus-months x (u/->long n)))
   "minusMonths"   (fn [x n] (ym-plus-months x (- (u/->long n))))
   "plusYears"     (fn [x n] (year-month (+ (ym-year x) (u/->long n)) (ym-month x)))
   "minusYears"    (fn [x n] (year-month (- (ym-year x) (u/->long n)) (ym-month x)))
   "withYear"      (fn [x v] (year-month (u/->long v) (ym-month x)))
   "withMonth"     (fn [x v] (year-month (ym-year x) (u/->long v)))
   "atDay"         (fn [x d] (l/local-date (days-from-civil (ym-year x) (ym-month x) (u/->long d))))
   "atEndOfMonth"  (fn [x] (l/local-date (days-from-civil (ym-year x) (ym-month x) (u/len-of-month (ym-year x) (ym-month x)))))
   "isValidDay"    (fn [x d] (let [dd (u/->long d)] (and (>= dd 1) (<= dd (u/len-of-month (ym-year x) (ym-month x))))))
   "isBefore"      (fn [x o] (neg? (ym-cmp x o)))
   "isAfter"       (fn [x o] (pos? (ym-cmp x o)))
   "compareTo"     ym-cmp
   "equals"        (fn [x o] (boolean (and (impl/jt? o) (ym? o) (= (ym-key x) (ym-key o)))))
   "hashCode"      (fn [x] (+ (* (ym-year x) 13) (ym-month x)))
   "toString"      ym->string})

(statics! ["YearMonth" "java.time.YearMonth"]
  {"of"    (fn [y m] (year-month (u/->long y) (u/->long m)))
   "now"   (fn [& _] (let [[y m _] (civil-from-days (u/floor-div (System/currentTimeMillis) 86400000))] (year-month y m)))
   "parse" (fn [s & _] (let [d (str s)]
                         (year-month (or (u/digits-at d 0 4) (throw (ex-info "could not parse YearMonth" {})))
                                     (or (u/digits-at d 5 2) (throw (ex-info "could not parse YearMonth" {}))))))})
