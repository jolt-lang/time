(ns jolt.time.number-data
  "Per-locale number and currency symbols, measured from the reference JVM.

  Feeds jolt core's :number-symbols and :currency-data extension points, which
  core declares with ROOT values only because it carries no locale data of its
  own. Ids are BCP-47-ish lang-COUNTRY; the empty id is ROOT.

  Note a language-only id has no country and therefore no currency: the JVM
  renders de as the generic currency sign, and only a country id such as
  de-DE carries a real symbol. Both are here, as measured -- every currency
  entry was checked by rebuilding the JVM's formatted output from these fields."
  (:require [jolt.time.locale-data :as ld]))

(def number-symbols
  {""       {:decimal-sep "." :grouping-sep ","}
   "de"     {:decimal-sep "," :grouping-sep "."}
   "de-CH"  {:decimal-sep "." :grouping-sep "'"}
   "de-DE"  {:decimal-sep "," :grouping-sep "."}
   "en"     {:decimal-sep "." :grouping-sep ","}
   "en-GB"  {:decimal-sep "." :grouping-sep ","}
   "en-US"  {:decimal-sep "." :grouping-sep ","}
   "es"     {:decimal-sep "," :grouping-sep "."}
   "es-ES"  {:decimal-sep "," :grouping-sep "."}
   "fr"     {:decimal-sep "," :grouping-sep " "}
   "fr-FR"  {:decimal-sep "," :grouping-sep " "}
   "it"     {:decimal-sep "," :grouping-sep "."}
   "it-IT"  {:decimal-sep "," :grouping-sep "."}
   "ja"     {:decimal-sep "." :grouping-sep ","}
   "ja-JP"  {:decimal-sep "." :grouping-sep ","}
   "ko"     {:decimal-sep "." :grouping-sep ","}
   "ko-KR"  {:decimal-sep "." :grouping-sep ","}
   "nl"     {:decimal-sep "," :grouping-sep "."}
   "nl-NL"  {:decimal-sep "," :grouping-sep "."}
   "pt"     {:decimal-sep "," :grouping-sep "."}
   "pt-BR"  {:decimal-sep "," :grouping-sep "."}
   "ru"     {:decimal-sep "," :grouping-sep " "}
   "ru-RU"  {:decimal-sep "," :grouping-sep " "}
   "zh"     {:decimal-sep "." :grouping-sep ","}
   "zh-CN"  {:decimal-sep "." :grouping-sep ","}})

(def currency
  {""       {:symbol "¤", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "de"     {:symbol "¤", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "de-CH"  {:symbol "CHF", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep "'"}
   "de-DE"  {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "en"     {:symbol "¤", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-GB"  {:symbol "£", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-US"  {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es"     {:symbol "¤", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "es-ES"  {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "fr"     {:symbol "¤", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-FR"  {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "it"     {:symbol "¤", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "it-IT"  {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "ja"     {:symbol "¤", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "ja-JP"  {:symbol "￥", :symbol-sep "", :symbol-first? true, :frac-digits 0, :decimal-sep ".", :grouping-sep ","}
   "ko"     {:symbol "¤", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "ko-KR"  {:symbol "₩", :symbol-sep "", :symbol-first? true, :frac-digits 0, :decimal-sep ".", :grouping-sep ","}
   "nl"     {:symbol "¤", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "nl-NL"  {:symbol "€", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "pt"     {:symbol "¤", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "pt-BR"  {:symbol "R$", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "ru"     {:symbol "¤", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "ru-RU"  {:symbol "₽", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "zh"     {:symbol "¤", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "zh-CN"  {:symbol "¥", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}})

(defn symbols-for
  "Number symbols for a locale id, narrowed the same way the pattern tables are."
  [id]
  (get number-symbols (ld/resolve-id id) (get number-symbols "")))

(defn currency-for
  "Currency data for a locale id, narrowed the same way."
  [id]
  (get currency (ld/resolve-id id) (get currency "")))
