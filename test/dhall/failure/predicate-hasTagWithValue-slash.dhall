let P = ../../../dhall/Predicate/package.dhall

let tag = ../../../dhall/Tags/tag.dhall

in  P.hasTagWithValue (tag "test") [ "va/l1", "val2" ]
