(ns jolt.time.diag (:require [jolt.time]))
(defn- cm [e] (let [r (resolve 'jolt.host/condition-message)] (if r ((deref r) e) (ex-message e))))
(defn -main [& _]
  (println :ls (try (load-string "(str (.toString (.atOffset (java.time.LocalDateTime/of 2020 3 5 13 45 30) (java.time.ZoneOffset/ofHours -5))))") (catch Throwable e (str "ERR " (cm e)))))
  (println :inst-ofp (try (load-string "(string? (.format (java.time.format.DateTimeFormatter/ofPattern \"yyyy-MM-dd\") #inst \"2020-03-05T13:45:30Z\"))") (catch Throwable e (str "ERR " (cm e)))))
  (System/exit 0))
