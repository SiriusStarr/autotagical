let between = ../../../dhall/Internal/Predicate/between

let Bound = ../../../dhall/Internal/Predicate/Bound

let P = ../../../dhall/Predicate/package.dhall

in  between Bound.AllPredicates (Bound.Bound 99) [ P.always ]
