# jolt-lang/time

Date and time for [jolt](https://github.com/jolt-lang/jolt) via
[juxt/tick](https://github.com/juxt/tick).

The library provides the `java.time.*` surface (`Instant`, `LocalDate`,
`ZonedDateTime`, `Duration`, `DateTimeFormatter`, …) as a pure-Clojure host
shim, then pulls tick and cljc.java-time through `deps.edn` to expose tick's
idiomatic Clojure API on top. Requiring it installs the shim and gives you the
tick API.

## Use

```clojure
;; deps.edn
{:deps {io.github.jolt-lang/time {:git/url "https://github.com/jolt-lang/time.git"
                                  :git/sha "…"}}}
```

```clojure
(require '[tick.core :as t])
(t/now)
(t/>> (t/date "2020-01-01") (t/new-period 3 :months))
```

## Test

```
joltc -M:test
```

Runs tick's own suite plus the migrated jolt `java.time` cases. tick, spec.alpha,
and the time-literals data-reader glue are vendored under `vendor/` so the gate
is self-contained.
