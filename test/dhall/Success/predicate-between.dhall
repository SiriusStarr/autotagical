let P = ../../../dhall/Predicate/package.dhall

in  P.between 0 2 [ P.always, P.not P.always ]
