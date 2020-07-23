let P = ../../../dhall/Predicate/package.dhall

let tag = ../../../dhall/Tags/tag.dhall

in  P.hasTag (tag "test")
