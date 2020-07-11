let F = ../../../dhall/Renaming/Name/Component/package.dhall

let always = ../../../dhall/Predicate/always

in  F.when always [ F.originalName ]
