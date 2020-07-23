let P = ../../../dhall/Predicate/package.dhall

let tag = ../../../dhall/Tags/tag.dhall

in  P.hasTagWithValue (tag "test") [ "val1", "val2" ]
