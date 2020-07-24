{-|
# Config.With.LogLevel.info

## Default Behavior

By default, `autotagical` only reports warnings, which are unexpected
situations or cases where files could not be sorted, moved, renamed, etc.

## Option Behavior

This option set logging level to `Info`.  This is useful if you're trying to
debug your `Predicate`s, `InputFormat`, etc. It's often helpful to combine this
with `Config.With.dryRun`, if you'd like to check out the behavior of your
settings without actually manipulating files.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in Autotagical::{...} ⫽ Autotagical.Config.With.LogLevel.info
```

-}

let LogLevel =
        ../../../Internal/Logging/LogLevel sha256:8b0ef97f0f052a52dd67e11fa031d0efe3ff18c020f9911ab2179066dee3922e
      ? ../../../Internal/Logging/LogLevel

in  { logLevel = LogLevel.Info } : { logLevel : LogLevel }
