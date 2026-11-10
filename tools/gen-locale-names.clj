;; Regenerates the data in src/jolt/time/locale_names.clj: Locale display names
;; for the display locales this library carries, read off the JDK it runs on.
;;
;;   JAVA_CMD=/opt/homebrew/opt/openjdk@21/bin/java clojure -M tools/gen-locale-names.clj
;;
;; Run it on the JDK the conformance oracle is pinned to (JDK 21).
(import '[java.util Locale])
(require '[clojure.string :as str])
;; the bundled display languages; zh-Hant is Taiwan's Traditional Chinese and
;; zh-Hant-HK Hong Kong's (jolt.time.locale-names maps zh-TW / zh-HK / zh-MO onto
;; them). Each country of these languages the JDK has a locale for whose names
;; differ from its language's is a table of those differences (fr-CA, es-MX, …).
(def base-disp ["" "en" "de" "fr" "es" "it" "ja" "ko" "nl" "pt" "ru" "zh" "zh-Hant" "zh-Hant-HK"])
(def base-langs (set (remove #(or (= % "") (.contains ^String % "-")) base-disp)))
;; every code the JDK has a locale for, not only the ISO lists
(def av (Locale/getAvailableLocales))
(def langs (sort (distinct (concat (Locale/getISOLanguages) (remove empty? (map #(.getLanguage %) av))))))
(def countries (sort (distinct (concat (Locale/getISOCountries) (remove empty? (map #(.getCountry %) av))))))
;; scripts: no API lists the codes the JDK can name, so ask it of every
;; four-letter code and keep the ones it names in English
(def scripts
  (let [en (Locale/forLanguageTag "en") az (map char (range (int \a) (inc (int \z))))]
    (sort (for [a az b az c az d az
                :let [code (str (Character/toUpperCase (char a)) b c d)
                      n (try (.getDisplayScript (.build (.setScript (java.util.Locale$Builder.) code)) en) (catch Exception _ ""))]
                :when (and (seq n) (not= n code))]
            code))))
(def variants (sort (distinct (remove empty? (map #(.getVariant %) av)))))
(defn names [dl f codes]
  (into (sorted-map) (keep (fn [c] (let [n (f c dl)] (when (and (seq n) (not= n c)) [c n]))) codes)))
(defn tables [d]
  (let [dl (Locale/forLanguageTag (if (= d "") "und" d))
        tw (Locale/forLanguageTag "zh-Hant-TW")
        shown (.getDisplayName tw dl) script (.getDisplayScript tw dl)]
    {:languages (names dl #(.getDisplayLanguage (Locale. %1) %2) langs)
     :countries (names dl #(.getDisplayCountry (Locale. "" %1) %2) countries)
     :scripts (names dl #(.getDisplayScript (.build (.setScript (java.util.Locale$Builder.) %1)) %2) scripts)
     :variants (names dl #(.getDisplayVariant (Locale. "en" "US" %1) %2) variants)
     ;; how the locale joins the parts in parentheses
     :separator (subs shown (+ (.indexOf shown script) (count script)) (.indexOf shown (.getDisplayCountry tw dl)))}))
(defn enc [m] (str/join "|" (map (fn [[k v]] (str k "=" v)) m)))
;; ROOT is stored as its differences from en: a name en has and ROOT does not is
;; written as the code itself
(defn root-diff [root en]
  (into (sorted-map)
        (concat (remove (fn [[k v]] (= v (get en k))) root)
                (for [k (keys en) :when (not (contains? root k))] [k k]))))
(def en (tables "en"))
;; a country table: only what differs from its language's (a name the language
;; has and the country does not is written as the code, as for ROOT)
(defn parent-of [d] (subs d 0 (.lastIndexOf ^String d "-")))
(def country-disp
  (sort (distinct
          (for [l (Locale/getAvailableLocales)
                :let [lang (.getLanguage l) c (.getCountry l)]
                :when (and (base-langs lang) (not= lang "zh") (re-matches #"[A-Z]{2}|[0-9]{3}" c)
                           (empty? (.getScript l)) (empty? (.getVariant l)))]
            (str lang "-" c)))))
(def base-tables (into {} (for [d base-disp] [d (tables d)])))
(def out
  (into (sorted-map)
    (concat
      (for [d base-disp :let [t (base-tables d)]]
        [d (assoc (into {} (for [k [:languages :countries :scripts :variants]]
                             [k (enc (if (= d "") (root-diff (get t k) (get en k)) (get t k)))]))
                  :separator (:separator t))])
      (for [d country-disp :let [t (tables d) p (base-tables (parent-of d))
                                 diffs (into {} (for [k [:languages :countries :scripts :variants]] [k (root-diff (get t k) (get p k))]))]
            :when (or (some seq (vals diffs)) (not= (:separator t) (:separator p)))]
        [d (assoc (into {} (for [[k v] diffs] [k (enc v)])) :separator (:separator t) :parent (parent-of d))]))))
(let [p "src/jolt/time/locale_names.clj" s (slurp p)
      i (.indexOf s "(def ^:private raw\n") j (.indexOf s ")\n\n(defn- parse")]
  (spit p (str (subs s 0 (+ i (count "(def ^:private raw\n"))) (pr-str out) (subs s j)))
  (println "wrote" (count out) "display locales"))
