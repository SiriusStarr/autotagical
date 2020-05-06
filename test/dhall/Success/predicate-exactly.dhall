let P = ../../../dhall/Predicate/package.dhall

in  P.exactly 1 [ P.always, P.not P.always ]
