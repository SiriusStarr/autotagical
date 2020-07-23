{-
# Config.With.ignorePatterns

## Default Behavior

By default, `autotagical` will load all files in the input folders that match
the `inputPatterns`.

## Option Behavior

This option specifies additional patterns to be specifically *excluded* from the
files otherwise input.  This is useful for cases in which it's easier to specify
the files one cares about by exclusion rather than inclusion.

**Note:** Ignored files are not scanned for being unnamed (because they are
ignored).

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in    Autotagical::{...}
    ⫽ Autotagical.Config.With.ignorePatterns
        (Autotagical.Glob.patterns [ "*.png", "*.jpg" ])
```

-}

let GlobPatterns =
        ../../Internal/GlobPatterns/Type sha256:26a29e0113646fb623fba2a6657b31b99127b689d510ef6761df7dd49da8a5bb
      ? ../../Internal/GlobPatterns/Type

let ignorePatterns
    : GlobPatterns → { ignorePatterns : Optional GlobPatterns }
    = λ(patterns : GlobPatterns) → { ignorePatterns = Some patterns }

in  ignorePatterns
