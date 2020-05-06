let P = ../../../dhall/Predicate/package.dhall

let tag = ../../../dhall/Tag/tag

in  P.hasTag (tag "test")
