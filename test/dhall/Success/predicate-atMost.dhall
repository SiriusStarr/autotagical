let P = ../../../dhall/Predicate/package.dhall

in  P.atMost 1 [ P.always, P.not P.always ]
