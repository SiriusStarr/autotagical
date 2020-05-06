let P = ../../../dhall/Predicate/package.dhall

in  P.atLeast 2 [ P.always, P.not P.always ]
