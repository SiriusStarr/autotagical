{-
# Config.With.LogTo.file

## Default Behavior

By default, `autotagical` writes logs to `stdout`.

## Option Behavior

Given a (relative or absolute) path to a file, this option writes log messages
to that file.  Note that this will **append** to the file if the file already
exists, so be careful where you point it.

Internally, this uses [`Control.Logging`](https://hackage.haskell.org/package/logging-3.0.5/docs/Control-Logging.html#v:withFileLogging)
(and by extension [`System.Log.FastLogger`](https://hackage.haskell.org/package/fast-logger-3.0.1/docs/System-Log-FastLogger.html#v:newFileLoggerSet)).

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in Autotagical::{...} ⫽ Autotagical.Config.With.LogTo.file "autotagical.log"
```

-}

let LogDestination =
        ../../../Internal/Logging/LogDestination sha256:47393d4c5bdc46697670d9b6d4c73c4875c1df2f2cf2f1cbe68b2afe03c0816f
      ? ../../../Internal/Logging/LogDestination

let file
    : Text → { logDestination : LogDestination }
    = λ(path : Text) → { logDestination = LogDestination.File path }

in  file
