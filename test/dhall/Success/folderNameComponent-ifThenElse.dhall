let F = ../../../dhall/Sorting/Folder/Name/Component/package.dhall

let always = ../../../dhall/Predicate/always

in  F.ifThenElse always [ F.text "test" ] [ F.originalName ]
