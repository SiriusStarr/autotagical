let P = ../../../dhall/Predicate/package.dhall

let tag = ../../../dhall/Tag/tag

in  P.hasTagWithValue (tag "test") ([] : List Text)
