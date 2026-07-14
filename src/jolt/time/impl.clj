(ns jolt.time.impl
  "Machinery shared by the java.time value types.

  A value is an opaque host table tagged with its type keyword and carrying its
  canonical fields. Equality, hashing, str/pr rendering, ordering, and instance?
  are supplied per type through jolt's value-semantics seams, so a java.time
  value compares, prints, and sorts like the real thing without being a map."
  (:require [jolt.host :as host]))

;; --- civil <-> days since the Unix epoch (Howard Hinnant's algorithms) -------
;; No portable UTC mktime, so epoch days are computed directly from y/m/d.

(defn days-from-civil
  "Days from 1970-01-01 to the proleptic-Gregorian date y/m/d."
  [y m d]
  (let [y2  (if (<= m 2) (dec y) y)
        era (quot (if (>= y2 0) y2 (- y2 399)) 400)
        yoe (- y2 (* era 400))
        doy (+ (quot (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (dec d))
        doe (+ (* yoe 365) (quot yoe 4) (- (quot yoe 100)) doy)]
    (+ (* era 146097) doe -719468)))

(defn civil-from-days
  "Inverse of days-from-civil; returns [year month day]."
  [z]
  (let [z2  (+ z 719468)
        era (quot (if (>= z2 0) z2 (- z2 146096)) 146097)
        doe (- z2 (* era 146097))
        yoe (quot (+ doe (- (quot doe 1460)) (quot doe 36524) (- (quot doe 146096))) 365)
        y   (+ yoe (* era 400))
        doy (- doe (+ (* 365 yoe) (quot yoe 4) (- (quot yoe 100))))
        mp  (quot (+ (* 5 doy) 2) 153)
        d   (+ (- doy (quot (+ (* 153 mp) 2) 5)) 1)
        m   (+ mp (if (< mp 10) 3 -9))]
    [(if (<= m 2) (inc y) y) m d]))

;; --- value machinery ---------------------------------------------------------

;; type-kw -> {:eq (fn [a b]) :hash (fn [v]) :str (fn [v]) :cmp (fn [a b]|nil)
;;             :classes #{class-name-strings}}
(defonce ^:private registry (atom {}))

(def ^:private type-key :jolt.time/type)

(defn register-type!
  "Register value semantics for a java.time type keyword. :cmp may be nil for an
  unordered type; :classes lists every class/interface name the type satisfies
  (its own plus supertypes like Temporal/Comparable), simple and fully qualified."
  [type-kw spec]
  (swap! registry assoc type-kw spec))

(defn value
  "An opaque value of `type-kw` carrying `fields` (a map)."
  [type-kw fields]
  (let [t (host/tagged-table type-kw)]
    (host/ref-put! t type-key type-kw)
    (reduce-kv (fn [_ k v] (host/ref-put! t k v)) nil fields)
    t))

(defn field [v k] (host/ref-get v k))
(defn type-of [v] (and (host/table? v) (host/ref-get v type-key)))

(defn jt?
  "Is v one of our java.time values?"
  [v]
  (boolean (when-let [tk (type-of v)] (contains? @registry tk))))

(defn- spec-of [v] (get @registry (type-of v)))

(defn- same-type? [a b]
  (and (jt? a) (jt? b) (= (type-of a) (type-of b))))

;; --- the seams (installed once) ----------------------------------------------

(defn- install-seams! []
  (__register-eq!
   (fn [a b] (or (jt? a) (jt? b)))
   (fn [a b] (boolean (and (same-type? a b) ((:eq (spec-of a)) a b)))))
  (__register-hash! jt? (fn [v] ((:hash (spec-of v)) v)))
  ;; java.time values have no reader literal, so pr renders the same as str
  ;; (time-literals layers #time/… readable printing on top separately).
  (__register-str! jt? (fn [v] ((:str (spec-of v)) v)))
  (__register-pr!  jt? (fn [v] ((:str (spec-of v)) v)))
  (__register-compare!
   (fn [a b] (or (jt? a) (jt? b)))
   (fn [a b]
     (when-not (same-type? a b)
       (throw (ex-info "compare: incomparable time values" {:a a :b b})))
     (let [cmp (:cmp (spec-of a))]
       (when-not cmp
         (throw (ex-info "compare: unordered time type" {:type (type-of a)})))
       (cmp a b))))
  (__register-instance-check!
   (fn [class-name v]
     (when (jt? v)
       (boolean (contains? (:classes (spec-of v)) class-name))))))

(defonce ^:private installed (install-seams!))
