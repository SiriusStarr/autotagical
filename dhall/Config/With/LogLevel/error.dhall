{-
# Config.With.LogLevel.error

## Default Behavior

By default, `autotagical` only reports warnings, which are unexpected
situations or cases where files could not be sorted, moved, renamed, etc.

## Option Behavior

This option sets logging level to `Error`.  This isn't recommended, unless you
know warnings are going to be thrown and you're fine with ignoring that.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in Autotagical::{...} ⫽ Autotagical.Config.With.LogLevel.error
```

-}

let LogLevel =
        ../../../Internal/Logging/LogLevel sha256:8b0ef97f0f052a52dd67e11fa031d0efe3ff18c020f9911ab2179066dee3922e
      ? ../../../Internal/Logging/LogLevel

in  { logLevel = LogLevel.Error } : { logLevel : LogLevel }
