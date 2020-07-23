let P = ../../../dhall/Predicate/package.dhall

let Predicate = ../../../dhall/Internal/Predicate/Type

in  P.between 0 1 ([] : List Predicate)
