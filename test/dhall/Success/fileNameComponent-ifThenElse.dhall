let F = ../../../dhall/RenamingSchema/Name/Component/package.dhall

let always = ../../../dhall/Predicate/always

in  F.ifThenElse always [ F.text "test" ] [ F.originalName ]
