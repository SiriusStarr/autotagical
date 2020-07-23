let F = ../../../dhall/Renaming/Name/Component/package.dhall

let always = ../../../dhall/Predicate/always.dhall

in  F.ifThenElse always [ F.text "test" ] [ F.originalName ]
