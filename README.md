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
                                  :git/sha "2738160a958345a927e83cdb8f5a80a2c47f0aa0"}}}
```

```clojure
(require '[tick.core :as t])
(t/now)
(t/>> (t/date "2020-01-01") (t/new-period 3 :months))
```

## Native libraries and time zones

This library declares no shared libraries of its own — there is no
`:jolt/native` entry in `deps.edn`, and nothing extra to install. It runs on
jolt's built-in host primitives.

Named IANA zones (`America/New_York`, `Europe/Paris`, …) have their offset
resolved by the core `jolt.host/tz-offset-seconds` primitive, which the jolt
host implements over libc. If that primitive is unavailable, those zones fall
back to a built-in DST rule table covering the US, EU, AU, and NZ families.
Fixed offsets (`Z`, `+05:30`) resolve purely in Clojure and never touch libc.

## Test

```
jolt -M:test
```

Runs tick's own suite plus the migrated jolt `java.time` cases. tick, spec.alpha,
and the time-literals data-reader glue are vendored under `vendor/` so the gate
is self-contained.
