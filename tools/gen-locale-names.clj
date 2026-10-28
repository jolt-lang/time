;; Regenerates the data in src/jolt/time/locale_names.clj: Locale display names
;; for the display locales this library carries, read off the JDK it runs on.
;;
;;   JAVA_CMD=/opt/homebrew/opt/openjdk@21/bin/java clojure -M tools/gen-locale-names.clj
;;
;; Run it on the JDK the conformance oracle is pinned to (JDK 21).
(import '[java.util Locale])
(require '[clojure.string :as str])
(def disp ["" "en" "de" "fr" "es" "it" "ja" "ko" "nl" "pt" "ru" "zh"])
(def langs (sort (Locale/getISOLanguages)))
(def countries (sort (Locale/getISOCountries)))
(def scripts ["Arab" "Cyrl" "Deva" "Grek" "Hans" "Hant" "Hebr" "Jpan" "Kore" "Latn" "Thai"])
(def variants ["POSIX"])
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
(def out
  (into (sorted-map)
    (for [d disp :let [t (tables d)]]
      [d (assoc (into {} (for [k [:languages :countries :scripts :variants]]
                           [k (enc (if (= d "") (root-diff (get t k) (get en k)) (get t k)))]))
                :separator (:separator t))])))
(let [p "src/jolt/time/locale_names.clj" s (slurp p)
      i (.indexOf s "(def ^:private raw\n") j (.indexOf s ")\n\n(defn- parse")]
  (spit p (str (subs s 0 (+ i (count "(def ^:private raw\n"))) (pr-str out) (subs s j)))
  (println "wrote" (count disp) "display locales"))
