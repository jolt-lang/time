(ns jolt.time.locale-data
  "Localized date/time format data measured from the reference JVM's CLDR, plus
  the lookups over it. Per locale id: :patterns keyed by [kind style] with kind
  :date/:time/:date-time and style :short/:medium/:long/:full, and :months,
  :months-short, :days, :days-short name tables (months January-first, days
  Monday-first). Ids are BCP-47-ish \"lang-COUNTRY\"; \"\" is ROOT. ROOT's wide
  month/day names are the full English names the reference JVM renders (Selmer
  accepts either CLDR shape).")

(def locales
  {""
 {:patterns
  {[:date-time :full] "y MMMM d, EEEE HH:mm:ss zzzz",
   [:date-time :long] "y MMMM d HH:mm:ss z",
   [:date :full] "y MMMM d, EEEE",
   [:time :medium] "HH:mm:ss",
   [:date :short] "y-MM-dd",
   [:time :full] "HH:mm:ss zzzz",
   [:time :long] "HH:mm:ss z",
   [:date :medium] "y MMM d",
   [:date :long] "y MMMM d",
   [:date-time :short] "y-MM-dd HH:mm",
   [:time :short] "HH:mm",
   [:date-time :medium] "y MMM d HH:mm:ss"},
  :months
  ["January"
   "February"
   "March"
   "April"
   "May"
   "June"
   "July"
   "August"
   "September"
   "October"
   "November"
   "December"],
  :months-short
  ["Jan"
   "Feb"
   "Mar"
   "Apr"
   "May"
   "Jun"
   "Jul"
   "Aug"
   "Sep"
   "Oct"
   "Nov"
   "Dec"],
  :days
  ["Monday"
   "Tuesday"
   "Wednesday"
   "Thursday"
   "Friday"
   "Saturday"
   "Sunday"],
  :days-short ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]},
 "nl"
 {:patterns
  {[:date-time :full] "EEEE d MMMM y, HH:mm:ss zzzz",
   [:date-time :long] "d MMMM y, HH:mm:ss z",
   [:date :full] "EEEE d MMMM y",
   [:time :medium] "HH:mm:ss",
   [:date :short] "dd-MM-y",
   [:time :full] "HH:mm:ss zzzz",
   [:time :long] "HH:mm:ss z",
   [:date :medium] "d MMM y",
   [:date :long] "d MMMM y",
   [:date-time :short] "dd-MM-y, HH:mm",
   [:time :short] "HH:mm",
   [:date-time :medium] "d MMM y, HH:mm:ss"},
  :months
  ["januari"
   "februari"
   "maart"
   "april"
   "mei"
   "juni"
   "juli"
   "augustus"
   "september"
   "oktober"
   "november"
   "december"],
  :months-short
  ["jan"
   "feb"
   "mrt"
   "apr"
   "mei"
   "jun"
   "jul"
   "aug"
   "sep"
   "okt"
   "nov"
   "dec"],
  :days
  ["maandag"
   "dinsdag"
   "woensdag"
   "donderdag"
   "vrijdag"
   "zaterdag"
   "zondag"],
  :days-short ["ma" "di" "wo" "do" "vr" "za" "zo"]},
 "pt"
 {:patterns
  {[:date-time :full] "EEEE, d 'de' MMMM 'de' y HH:mm:ss zzzz",
   [:date-time :long] "d 'de' MMMM 'de' y HH:mm:ss z",
   [:date :full] "EEEE, d 'de' MMMM 'de' y",
   [:time :medium] "HH:mm:ss",
   [:date :short] "dd/MM/y",
   [:time :full] "HH:mm:ss zzzz",
   [:time :long] "HH:mm:ss z",
   [:date :medium] "d 'de' MMM 'de' y",
   [:date :long] "d 'de' MMMM 'de' y",
   [:date-time :short] "dd/MM/y HH:mm",
   [:time :short] "HH:mm",
   [:date-time :medium] "d 'de' MMM 'de' y HH:mm:ss"},
  :months
  ["janeiro"
   "fevereiro"
   "março"
   "abril"
   "maio"
   "junho"
   "julho"
   "agosto"
   "setembro"
   "outubro"
   "novembro"
   "dezembro"],
  :months-short
  ["jan."
   "fev."
   "mar."
   "abr."
   "mai."
   "jun."
   "jul."
   "ago."
   "set."
   "out."
   "nov."
   "dez."],
  :days
  ["segunda-feira"
   "terça-feira"
   "quarta-feira"
   "quinta-feira"
   "sexta-feira"
   "sábado"
   "domingo"],
  :days-short ["seg." "ter." "qua." "qui." "sex." "sáb." "dom."]},
 "en"
 {:patterns
  {[:date-time :full] "EEEE, MMMM d, y, h:mm:ss a zzzz",
   [:date-time :long] "MMMM d, y, h:mm:ss a z",
   [:date :full] "EEEE, MMMM d, y",
   [:time :medium] "h:mm:ss a",
   [:date :short] "M/d/yy",
   [:time :full] "h:mm:ss a zzzz",
   [:time :long] "h:mm:ss a z",
   [:date :medium] "MMM d, y",
   [:date :long] "MMMM d, y",
   [:date-time :short] "M/d/yy, h:mm a",
   [:time :short] "h:mm a",
   [:date-time :medium] "MMM d, y, h:mm:ss a"},
  :months
  ["January"
   "February"
   "March"
   "April"
   "May"
   "June"
   "July"
   "August"
   "September"
   "October"
   "November"
   "December"],
  :months-short
  ["Jan"
   "Feb"
   "Mar"
   "Apr"
   "May"
   "Jun"
   "Jul"
   "Aug"
   "Sep"
   "Oct"
   "Nov"
   "Dec"],
  :days
  ["Monday"
   "Tuesday"
   "Wednesday"
   "Thursday"
   "Friday"
   "Saturday"
   "Sunday"],
  :days-short ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]},
 "zh"
 {:patterns
  {[:date-time :full] "y年M月d日EEEE zzzz HH:mm:ss",
   [:date-time :long] "y年M月d日 z HH:mm:ss",
   [:date :full] "y年M月d日EEEE",
   [:time :medium] "HH:mm:ss",
   [:date :short] "y/M/d",
   [:time :full] "zzzz HH:mm:ss",
   [:time :long] "z HH:mm:ss",
   [:date :medium] "y年M月d日",
   [:date :long] "y年M月d日",
   [:date-time :short] "y/M/d HH:mm",
   [:time :short] "HH:mm",
   [:date-time :medium] "y年M月d日 HH:mm:ss"},
  :months
  ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"],
  :months-short
  ["1月" "2月" "3月" "4月" "5月" "6月" "7月" "8月" "9月" "10月" "11月" "12月"],
  :days ["星期一" "星期二" "星期三" "星期四" "星期五" "星期六" "星期日"],
  :days-short ["周一" "周二" "周三" "周四" "周五" "周六" "周日"]},
 "it"
 {:patterns
  {[:date-time :full] "EEEE d MMMM y HH:mm:ss zzzz",
   [:date-time :long] "d MMMM y HH:mm:ss z",
   [:date :full] "EEEE d MMMM y",
   [:time :medium] "HH:mm:ss",
   [:date :short] "dd/MM/yy",
   [:time :full] "HH:mm:ss zzzz",
   [:time :long] "HH:mm:ss z",
   [:date :medium] "d MMM y",
   [:date :long] "d MMMM y",
   [:date-time :short] "dd/MM/yy, HH:mm",
   [:time :short] "HH:mm",
   [:date-time :medium] "d MMM y, HH:mm:ss"},
  :months
  ["gennaio"
   "febbraio"
   "marzo"
   "aprile"
   "maggio"
   "giugno"
   "luglio"
   "agosto"
   "settembre"
   "ottobre"
   "novembre"
   "dicembre"],
  :months-short
  ["gen"
   "feb"
   "mar"
   "apr"
   "mag"
   "giu"
   "lug"
   "ago"
   "set"
   "ott"
   "nov"
   "dic"],
  :days
  ["lunedì"
   "martedì"
   "mercoledì"
   "giovedì"
   "venerdì"
   "sabato"
   "domenica"],
  :days-short ["lun" "mar" "mer" "gio" "ven" "sab" "dom"]},
 "fr"
 {:patterns
  {[:date-time :full] "EEEE d MMMM y, HH:mm:ss zzzz",
   [:date-time :long] "d MMMM y, HH:mm:ss z",
   [:date :full] "EEEE d MMMM y",
   [:time :medium] "HH:mm:ss",
   [:date :short] "dd/MM/y",
   [:time :full] "HH:mm:ss zzzz",
   [:time :long] "HH:mm:ss z",
   [:date :medium] "d MMM y",
   [:date :long] "d MMMM y",
   [:date-time :short] "dd/MM/y HH:mm",
   [:time :short] "HH:mm",
   [:date-time :medium] "d MMM y, HH:mm:ss"},
  :months
  ["janvier"
   "février"
   "mars"
   "avril"
   "mai"
   "juin"
   "juillet"
   "août"
   "septembre"
   "octobre"
   "novembre"
   "décembre"],
  :months-short
  ["janv."
   "févr."
   "mars"
   "avr."
   "mai"
   "juin"
   "juil."
   "août"
   "sept."
   "oct."
   "nov."
   "déc."],
  :days
  ["lundi" "mardi" "mercredi" "jeudi" "vendredi" "samedi" "dimanche"],
  :days-short ["lun." "mar." "mer." "jeu." "ven." "sam." "dim."]},
 "de"
 {:patterns
  {[:date-time :full] "EEEE, d. MMMM y, HH:mm:ss zzzz",
   [:date-time :long] "d. MMMM y, HH:mm:ss z",
   [:date :full] "EEEE, d. MMMM y",
   [:time :medium] "HH:mm:ss",
   [:date :short] "dd.MM.yy",
   [:time :full] "HH:mm:ss zzzz",
   [:time :long] "HH:mm:ss z",
   [:date :medium] "dd.MM.y",
   [:date :long] "d. MMMM y",
   [:date-time :short] "dd.MM.yy, HH:mm",
   [:time :short] "HH:mm",
   [:date-time :medium] "dd.MM.y, HH:mm:ss"},
  :months
  ["Januar"
   "Februar"
   "März"
   "April"
   "Mai"
   "Juni"
   "Juli"
   "August"
   "September"
   "Oktober"
   "November"
   "Dezember"],
  :months-short
  ["Jan."
   "Feb."
   "März"
   "Apr."
   "Mai"
   "Juni"
   "Juli"
   "Aug."
   "Sept."
   "Okt."
   "Nov."
   "Dez."],
  :days
  ["Montag"
   "Dienstag"
   "Mittwoch"
   "Donnerstag"
   "Freitag"
   "Samstag"
   "Sonntag"],
  :days-short ["Mo." "Di." "Mi." "Do." "Fr." "Sa." "So."]},
 "ru"
 {:patterns
  {[:date-time :full] "EEEE, d MMMM y 'г'., HH:mm:ss zzzz",
   [:date-time :long] "d MMMM y 'г'., HH:mm:ss z",
   [:date :full] "EEEE, d MMMM y 'г'.",
   [:time :medium] "HH:mm:ss",
   [:date :short] "dd.MM.y",
   [:time :full] "HH:mm:ss zzzz",
   [:time :long] "HH:mm:ss z",
   [:date :medium] "d MMM y 'г'.",
   [:date :long] "d MMMM y 'г'.",
   [:date-time :short] "dd.MM.y, HH:mm",
   [:time :short] "HH:mm",
   [:date-time :medium] "d MMM y 'г'., HH:mm:ss"},
  :months
  ["января"
   "февраля"
   "марта"
   "апреля"
   "мая"
   "июня"
   "июля"
   "августа"
   "сентября"
   "октября"
   "ноября"
   "декабря"],
  :months-short
  ["янв."
   "февр."
   "мар."
   "апр."
   "мая"
   "июн."
   "июл."
   "авг."
   "сент."
   "окт."
   "нояб."
   "дек."],
  :days
  ["понедельник"
   "вторник"
   "среда"
   "четверг"
   "пятница"
   "суббота"
   "воскресенье"],
  :days-short ["пн" "вт" "ср" "чт" "пт" "сб" "вс"]},
 "es"
 {:patterns
  {[:date-time :full] "EEEE, d 'de' MMMM 'de' y, H:mm:ss (zzzz)",
   [:date-time :long] "d 'de' MMMM 'de' y, H:mm:ss z",
   [:date :full] "EEEE, d 'de' MMMM 'de' y",
   [:time :medium] "H:mm:ss",
   [:date :short] "d/M/yy",
   [:time :full] "H:mm:ss (zzzz)",
   [:time :long] "H:mm:ss z",
   [:date :medium] "d MMM y",
   [:date :long] "d 'de' MMMM 'de' y",
   [:date-time :short] "d/M/yy, H:mm",
   [:time :short] "H:mm",
   [:date-time :medium] "d MMM y, H:mm:ss"},
  :months
  ["enero"
   "febrero"
   "marzo"
   "abril"
   "mayo"
   "junio"
   "julio"
   "agosto"
   "septiembre"
   "octubre"
   "noviembre"
   "diciembre"],
  :months-short
  ["ene"
   "feb"
   "mar"
   "abr"
   "may"
   "jun"
   "jul"
   "ago"
   "sept"
   "oct"
   "nov"
   "dic"],
  :days
  ["lunes" "martes" "miércoles" "jueves" "viernes" "sábado" "domingo"],
  :days-short ["lun" "mar" "mié" "jue" "vie" "sáb" "dom"]},
 "en-US"
 {:patterns
  {[:date-time :full] "EEEE, MMMM d, y, h:mm:ss a zzzz",
   [:date-time :long] "MMMM d, y, h:mm:ss a z",
   [:date :full] "EEEE, MMMM d, y",
   [:time :medium] "h:mm:ss a",
   [:date :short] "M/d/yy",
   [:time :full] "h:mm:ss a zzzz",
   [:time :long] "h:mm:ss a z",
   [:date :medium] "MMM d, y",
   [:date :long] "MMMM d, y",
   [:date-time :short] "M/d/yy, h:mm a",
   [:time :short] "h:mm a",
   [:date-time :medium] "MMM d, y, h:mm:ss a"},
  :months
  ["January"
   "February"
   "March"
   "April"
   "May"
   "June"
   "July"
   "August"
   "September"
   "October"
   "November"
   "December"],
  :months-short
  ["Jan"
   "Feb"
   "Mar"
   "Apr"
   "May"
   "Jun"
   "Jul"
   "Aug"
   "Sep"
   "Oct"
   "Nov"
   "Dec"],
  :days
  ["Monday"
   "Tuesday"
   "Wednesday"
   "Thursday"
   "Friday"
   "Saturday"
   "Sunday"],
  :days-short ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]},
 "ja"
 {:patterns
  {[:date-time :full] "y年M月d日EEEE H時mm分ss秒 zzzz",
   [:date-time :long] "y年M月d日 H:mm:ss z",
   [:date :full] "y年M月d日EEEE",
   [:time :medium] "H:mm:ss",
   [:date :short] "y/MM/dd",
   [:time :full] "H時mm分ss秒 zzzz",
   [:time :long] "H:mm:ss z",
   [:date :medium] "y/MM/dd",
   [:date :long] "y年M月d日",
   [:date-time :short] "y/MM/dd H:mm",
   [:time :short] "H:mm",
   [:date-time :medium] "y/MM/dd H:mm:ss"},
  :months
  ["1月" "2月" "3月" "4月" "5月" "6月" "7月" "8月" "9月" "10月" "11月" "12月"],
  :months-short
  ["1月" "2月" "3月" "4月" "5月" "6月" "7月" "8月" "9月" "10月" "11月" "12月"],
  :days ["月曜日" "火曜日" "水曜日" "木曜日" "金曜日" "土曜日" "日曜日"],
  :days-short ["月" "火" "水" "木" "金" "土" "日"]},
 "en-GB"
 {:patterns
  {[:date-time :full] "EEEE, d MMMM y, HH:mm:ss zzzz",
   [:date-time :long] "d MMMM y, HH:mm:ss z",
   [:date :full] "EEEE, d MMMM y",
   [:time :medium] "HH:mm:ss",
   [:date :short] "dd/MM/y",
   [:time :full] "HH:mm:ss zzzz",
   [:time :long] "HH:mm:ss z",
   [:date :medium] "d MMM y",
   [:date :long] "d MMMM y",
   [:date-time :short] "dd/MM/y, HH:mm",
   [:time :short] "HH:mm",
   [:date-time :medium] "d MMM y, HH:mm:ss"},
  :months
  ["January"
   "February"
   "March"
   "April"
   "May"
   "June"
   "July"
   "August"
   "September"
   "October"
   "November"
   "December"],
  :months-short
  ["Jan"
   "Feb"
   "Mar"
   "Apr"
   "May"
   "Jun"
   "Jul"
   "Aug"
   "Sept"
   "Oct"
   "Nov"
   "Dec"],
  :days
  ["Monday"
   "Tuesday"
   "Wednesday"
   "Thursday"
   "Friday"
   "Saturday"
   "Sunday"],
  :days-short ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]},
 "ko"
 {:patterns
  {[:date-time :full] "y년 MMMM d일 EEEE a h시 m분 s초 zzzz",
   [:date-time :long] "y년 MMMM d일 a h시 m분 s초 z",
   [:date :full] "y년 MMMM d일 EEEE",
   [:time :medium] "a h:mm:ss",
   [:date :short] "yy. M. d.",
   [:time :full] "a h시 m분 s초 zzzz",
   [:time :long] "a h시 m분 s초 z",
   [:date :medium] "y. M. d.",
   [:date :long] "y년 MMMM d일",
   [:date-time :short] "yy. M. d. a h:mm",
   [:time :short] "a h:mm",
   [:date-time :medium] "y. M. d. a h:mm:ss"},
  :months
  ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"],
  :months-short
  ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"],
  :days ["월요일" "화요일" "수요일" "목요일" "금요일" "토요일" "일요일"],
  :days-short ["월" "화" "수" "목" "금" "토" "일"]}}
)

(defn- last-hyphen [s]
  (loop [i (dec (count s))]
    (cond (neg? i) nil
          (= \- (nth s i)) i
          :else (recur (dec i)))))

(defn- has-underscore? [s]
  (loop [i 0]
    (cond (>= i (count s)) false
          (= \_ (nth s i)) true
          :else (recur (inc i)))))

(defn resolve-id
  "Narrow a locale id to the most specific id the tables carry: \"zh-CN\" ->
  \"zh\" -> \"\". An id containing an underscore is a malformed language subtag
  (e.g. (Locale. \"en_US\")) and lands on ROOT, matching the JVM."
  [id]
  (let [id (or id "")]
    (cond
      (contains? locales id) id
      (has-underscore? id) ""
      :else (loop [s id]
              (if-let [i (last-hyphen s)]
                (let [p (subs s 0 i)]
                  (if (contains? locales p) p (recur p)))
                "")))))

(defn pattern
  "Localized pattern for [kind style]; kind :date/:time/:date-time, style
  :short/:medium/:long/:full. Always non-nil: ROOT carries all twelve."
  [id kind style]
  (get-in locales [(resolve-id id) :patterns [kind style]]))

(defn months [id full?] (get-in locales [(resolve-id id) (if full? :months :months-short)]))
(defn days [id full?] (get-in locales [(resolve-id id) (if full? :days :days-short)]))
