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
   "de-CH"  {:decimal-sep "." :grouping-sep "’"}
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
   "zh-CN"  {:decimal-sep "." :grouping-sep ","}
   ;; a country whose symbols differ from its language's, measured from JDK 21:
   ;; core falls a missed lang-COUNTRY back to the language (the :parent rule)
   "de-AT" {:decimal-sep ",", :grouping-sep " "}
   "de-LI" {:decimal-sep ".", :grouping-sep "’"}
   "en-AT" {:decimal-sep ",", :grouping-sep "."}
   "en-BE" {:decimal-sep ",", :grouping-sep "."}
   "en-CH" {:decimal-sep ".", :grouping-sep "’"}
   "en-DE" {:decimal-sep ",", :grouping-sep "."}
   "en-DK" {:decimal-sep ",", :grouping-sep "."}
   "en-FI" {:decimal-sep ",", :grouping-sep " "}
   "en-NL" {:decimal-sep ",", :grouping-sep "."}
   "en-SE" {:decimal-sep ",", :grouping-sep " "}
   "en-SI" {:decimal-sep ",", :grouping-sep "."}
   "en-ZA" {:decimal-sep ",", :grouping-sep " "}
   "es-BR" {:decimal-sep ".", :grouping-sep ","}
   "es-BZ" {:decimal-sep ".", :grouping-sep ","}
   "es-CR" {:decimal-sep ",", :grouping-sep " "}
   "es-CU" {:decimal-sep ".", :grouping-sep ","}
   "es-DO" {:decimal-sep ".", :grouping-sep ","}
   "es-GT" {:decimal-sep ".", :grouping-sep ","}
   "es-HN" {:decimal-sep ".", :grouping-sep ","}
   "es-MX" {:decimal-sep ".", :grouping-sep ","}
   "es-NI" {:decimal-sep ".", :grouping-sep ","}
   "es-PA" {:decimal-sep ".", :grouping-sep ","}
   "es-PE" {:decimal-sep ".", :grouping-sep ","}
   "es-PR" {:decimal-sep ".", :grouping-sep ","}
   "es-SV" {:decimal-sep ".", :grouping-sep ","}
   "es-US" {:decimal-sep ".", :grouping-sep ","}
   "fr-CA" {:decimal-sep ",", :grouping-sep " "}
   "fr-LU" {:decimal-sep ",", :grouping-sep "."}
   "fr-MA" {:decimal-sep ",", :grouping-sep "."}
   "it-CH" {:decimal-sep ".", :grouping-sep "’"}
   "pt-AO" {:decimal-sep ",", :grouping-sep " "}
   "pt-CH" {:decimal-sep ",", :grouping-sep " "}
   "pt-CV" {:decimal-sep ",", :grouping-sep " "}
   "pt-GQ" {:decimal-sep ",", :grouping-sep " "}
   "pt-GW" {:decimal-sep ",", :grouping-sep " "}
   "pt-LU" {:decimal-sep ",", :grouping-sep " "}
   "pt-MO" {:decimal-sep ",", :grouping-sep " "}
   "pt-MZ" {:decimal-sep ",", :grouping-sep " "}
   "pt-PT" {:decimal-sep ",", :grouping-sep " "}
   "pt-ST" {:decimal-sep ",", :grouping-sep " "}
   "pt-TL" {:decimal-sep ",", :grouping-sep " "}})

(def currency
  {""       {:symbol "¤", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "de"     {:symbol "¤", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "de-CH" {:symbol "CHF", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep "’"}
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
   "zh-CN"  {:symbol "¥", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   ;; every other country of these languages, measured from JDK 21 the same way
   "de-AT" {:symbol "€", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "de-BE" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "de-IT" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "de-LI" {:symbol "CHF", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep "’"}
   "de-LU" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "en-AE" {:symbol "AED", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-AG" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-AI" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-AS" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-AT" {:symbol "€", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "en-AU" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-BB" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-BE" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "en-BI" {:symbol "FBu", :symbol-sep "", :symbol-first? true, :frac-digits 0, :decimal-sep ".", :grouping-sep ","}
   "en-BM" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-BS" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-BW" {:symbol "P", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-BZ" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-CA" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-CC" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-CH" {:symbol "CHF", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep "’"}
   "en-CK" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-CM" {:symbol "FCFA", :symbol-sep "", :symbol-first? true, :frac-digits 0, :decimal-sep ".", :grouping-sep ","}
   "en-CX" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-CY" {:symbol "€", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-DE" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "en-DG" {:symbol "¤", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-DK" {:symbol "kr.", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "en-DM" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-ER" {:symbol "Nfk", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-FI" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "en-FJ" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-FK" {:symbol "£", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-FM" {:symbol "US$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-GD" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-GG" {:symbol "£", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-GH" {:symbol "GH₵", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-GI" {:symbol "£", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-GM" {:symbol "D", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-GU" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-GY" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-HK" {:symbol "HK$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-IE" {:symbol "€", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-IL" {:symbol "₪", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-IM" {:symbol "£", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-IN" {:symbol "₹", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-IO" {:symbol "US$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-JE" {:symbol "£", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-JM" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-KE" {:symbol "Ksh", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-KI" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-KN" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-KY" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-LC" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-LR" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-LS" {:symbol "LSL", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-MG" {:symbol "Ar", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-MH" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-MO" {:symbol "MOP$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-MP" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-MS" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-MT" {:symbol "€", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-MU" {:symbol "Rs", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-MV" {:symbol "Rf", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-MW" {:symbol "MK", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-MY" {:symbol "RM", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-NA" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-NF" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-NG" {:symbol "₦", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-NL" {:symbol "€", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "en-NR" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-NU" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-NZ" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-PG" {:symbol "K", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-PH" {:symbol "₱", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-PK" {:symbol "Rs", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-PN" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-PR" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-PW" {:symbol "US$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-RW" {:symbol "RF", :symbol-sep "", :symbol-first? true, :frac-digits 0, :decimal-sep ".", :grouping-sep ","}
   "en-SB" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-SC" {:symbol "SR", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-SD" {:symbol "SDG", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-SE" {:symbol "kr", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "en-SG" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-SH" {:symbol "£", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-SI" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "en-SL" {:symbol "Le", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-SS" {:symbol "£", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-SX" {:symbol "XCG", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-SZ" {:symbol "E", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-TC" {:symbol "US$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-TK" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-TO" {:symbol "T$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-TT" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-TV" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-TZ" {:symbol "TSh", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-UG" {:symbol "USh", :symbol-sep "", :symbol-first? true, :frac-digits 0, :decimal-sep ".", :grouping-sep ","}
   "en-UM" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-VC" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-VG" {:symbol "US$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-VI" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-VU" {:symbol "VT", :symbol-sep "", :symbol-first? true, :frac-digits 0, :decimal-sep ".", :grouping-sep ","}
   "en-WS" {:symbol "WS$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-ZA" {:symbol "R", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "en-ZM" {:symbol "K", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "en-ZW" {:symbol "ZWG", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-AR" {:symbol "$", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "es-BO" {:symbol "Bs", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "es-BR" {:symbol "R$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-BZ" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-CL" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 0, :decimal-sep ",", :grouping-sep "."}
   "es-CO" {:symbol "$", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "es-CR" {:symbol "₡", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "es-CU" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-DO" {:symbol "RD$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-EA" {:symbol "¤", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "es-EC" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "es-GQ" {:symbol "FCFA", :symbol-sep "", :symbol-first? true, :frac-digits 0, :decimal-sep ",", :grouping-sep "."}
   "es-GT" {:symbol "Q", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-HN" {:symbol "L", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-IC" {:symbol "¤", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "es-MX" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-NI" {:symbol "C$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-PA" {:symbol "B/.", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-PE" {:symbol "S/", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-PH" {:symbol "₱", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "es-PR" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-PY" {:symbol "Gs.", :symbol-sep " ", :symbol-first? true, :frac-digits 0, :decimal-sep ",", :grouping-sep "."}
   "es-SV" {:symbol "C", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-US" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "es-UY" {:symbol "$", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "es-VE" {:symbol "Bs.S", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "fr-BE" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-BF" {:symbol "F CFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-BI" {:symbol "FBu", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-BJ" {:symbol "F CFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-BL" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-CA" {:symbol "$", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-CD" {:symbol "FC", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-CF" {:symbol "FCFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-CG" {:symbol "FCFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-CH" {:symbol "CHF", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ".", :grouping-sep " "}
   "fr-CI" {:symbol "F CFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-CM" {:symbol "FCFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-DJ" {:symbol "Fdj", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-DZ" {:symbol "DA", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-GA" {:symbol "FCFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-GF" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-GN" {:symbol "FG", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-GP" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-GQ" {:symbol "FCFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-HT" {:symbol "G", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-KM" {:symbol "CF", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-LU" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "fr-MA" {:symbol "MAD", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "fr-MC" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-MF" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-MG" {:symbol "Ar", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-ML" {:symbol "F CFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-MQ" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-MR" {:symbol "UM", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-MU" {:symbol "Rs", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-NC" {:symbol "FCFP", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-NE" {:symbol "F CFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-PF" {:symbol "FCFP", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-PM" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-RE" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-RW" {:symbol "RF", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-SC" {:symbol "SR", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-SN" {:symbol "F CFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-SY" {:symbol "LS", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "fr-TD" {:symbol "FCFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-TG" {:symbol "F CFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-TN" {:symbol "DT", :symbol-sep " ", :symbol-first? false, :frac-digits 3, :decimal-sep ",", :grouping-sep " "}
   "fr-VU" {:symbol "VT", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-WF" {:symbol "FCFP", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "fr-YT" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "it-CH" {:symbol "SFr.", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep "’"}
   "it-SM" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "it-VA" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "ko-KP" {:symbol "KPW", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "nl-AW" {:symbol "Afl.", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "nl-BE" {:symbol "€", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "nl-BQ" {:symbol "$", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "nl-CW" {:symbol "XCG", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "nl-SR" {:symbol "$", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "nl-SX" {:symbol "XCG", :symbol-sep " ", :symbol-first? true, :frac-digits 2, :decimal-sep ",", :grouping-sep "."}
   "pt-AO" {:symbol "Kz", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "pt-CH" {:symbol "CHF", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "pt-CV" {:symbol "​", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "pt-GQ" {:symbol "FCFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "pt-GW" {:symbol "F CFA", :symbol-sep " ", :symbol-first? false, :frac-digits 0, :decimal-sep ",", :grouping-sep " "}
   "pt-LU" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "pt-MO" {:symbol "MOP$", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "pt-MZ" {:symbol "MTn", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "pt-PT" {:symbol "€", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "pt-ST" {:symbol "Db", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "pt-TL" {:symbol "US$", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "ru-BY" {:symbol "Br", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "ru-KG" {:symbol "сом", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "ru-KZ" {:symbol "₸", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "ru-MD" {:symbol "L", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "ru-UA" {:symbol "₴", :symbol-sep " ", :symbol-first? false, :frac-digits 2, :decimal-sep ",", :grouping-sep " "}
   "zh-HK" {:symbol "HK$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "zh-MO" {:symbol "MOP$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "zh-SG" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}
   "zh-TW" {:symbol "$", :symbol-sep "", :symbol-first? true, :frac-digits 2, :decimal-sep ".", :grouping-sep ","}})

(defn symbols-for
  "Number symbols for a locale id, narrowed the same way the pattern tables are."
  [id]
  (get number-symbols (ld/resolve-id id) (get number-symbols "")))

(defn currency-for
  "Currency data for a locale id, narrowed the same way."
  [id]
  (get currency (ld/resolve-id id) (get currency "")))
