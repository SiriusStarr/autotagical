{-
# Config.With.LogLevel.debug

## Default Behavior

By default, `autotagical` only reports warnings, which are unexpected
situations or cases where files could not be sorted, moved, renamed, etc.

## Option Behavior

This option sets logging level to `Debug`.  This is typically **extremely
excessive** and should generally not be used, unless you're trying to debug why
something isn't working internally as it should.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in Autotagical::{...} ⫽ Autotagical.Config.With.LogLevel.debug
```

-}

let LogLevel =
        ../../../Internal/Logging/LogLevel sha256:8b0ef97f0f052a52dd67e11fa031d0efe3ff18c020f9911ab2179066dee3922e
      ? ../../../Internal/Logging/LogLevel

in  { logLevel = LogLevel.Debug } : { logLevel : LogLevel }
