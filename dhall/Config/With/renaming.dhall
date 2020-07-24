{-|
# Config.With.renaming

## Default Behavior

By default, `autotagical` will only sort files, not attempt to rename unnamed
files.

## Option Behavior

This option instructs `autotagical` to rename files that match unnamed patterns
using a provided renaming schema.

**Note:** Ignored files are not scanned for being unnamed (because they are
ignored).

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in    Autotagical::{...}
    ⫽ Autotagical.Config.With.renaming
        (Autotagical.Glob.patterns [ "unnamed*" ])
        ( Autotagical.Renaming.schema
            [ Autotagical.Renaming.rule
                Autotagical.Predicate.always
                ( Autotagical.Renaming.Name.template
                    [ Autotagical.Renaming.Name.Component.text "Renamed Number:"
                    , Autotagical.Renaming.Name.Component.duplicateNumber ]
                )
            ]
        )
```

-}

let GlobPatterns =
        ../../Internal/GlobPatterns/Type sha256:26a29e0113646fb623fba2a6657b31b99127b689d510ef6761df7dd49da8a5bb
      ? ../../Internal/GlobPatterns/Type

let RenamingSchema =
        ../../Internal/RenamingSchema/Type sha256:10694e04e153916ee5907f3aef8882da66dbff54996ca2b54b97ae27e38fdefc
      ? ../../Internal/RenamingSchema/Type

let renaming
    : GlobPatterns →
      RenamingSchema →
        { renaming :
            Optional { patterns : GlobPatterns, schema : RenamingSchema }
        }
    = λ(patterns : GlobPatterns) →
      λ(schema : RenamingSchema) →
        { renaming = Some { patterns, schema } }

in  renaming
