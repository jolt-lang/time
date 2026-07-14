(ns jolt.time.diag (:require [jolt.time]) (:import [java.time ZonedDateTime LocalDate]))
(defn -main [& _]
  (println :slash (try (str (ZonedDateTime/parse "2020-07-06T10:59:13.417Z")) (catch Throwable e (str "ERR " ((deref (resolve 'jolt.host/condition-message)) e)))))
  (println :dot (try (str (. ZonedDateTime parse "2020-07-06T10:59:13.417Z")) (catch Throwable e (str "ERR " ((deref (resolve 'jolt.host/condition-message)) e)))))
  (println :dotLD (try (str (. LocalDate parse "2020-07-06")) (catch Throwable e (str "ERR " ((deref (resolve 'jolt.host/condition-message)) e)))))
  (System/exit 0))
