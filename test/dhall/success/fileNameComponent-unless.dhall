let F = ../../../dhall/Renaming/Name/Component/package.dhall

let always = ../../../dhall/Predicate/always.dhall

in  F.unless always [ F.originalName ]
