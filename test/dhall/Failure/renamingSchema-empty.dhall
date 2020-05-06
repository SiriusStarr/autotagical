let R = ../../../dhall/RenamingSchema/package.dhall

let Rule = ../../../dhall/Internal/RenamingSchema/RenamingRule

in  R.renamingSchema ([] : List Rule)
