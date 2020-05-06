let P = ../../../dhall/Predicate/package.dhall

in  P.all [ P.always, P.not P.always ]
