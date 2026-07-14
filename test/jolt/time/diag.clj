(ns jolt.time.diag (:require [jolt.time] [jolt.time.zoned :as zd] [jolt.time.instant :as inst] [jolt.time.zones :as z]))
(defn -main [& _]
  (println :pii (try (inst/parse-iso-instant "2020-12-15T12:00:10Z") (catch Throwable e (str "ERR " ((deref (resolve 'jolt.host/condition-message)) e)))))
  (println :zoi (try (str (zd/zoned-of-instant (inst/parse-iso-instant "2020-12-15T12:00:10Z") "Europe/London")) (catch Throwable e (str "ERR " ((deref (resolve 'jolt.host/condition-message)) e)))))
  (System/exit 0))
