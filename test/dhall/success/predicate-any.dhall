let P = ../../../dhall/Predicate/package.dhall

in  P.any [ P.always, P.not P.always ]
