let F = ../../../dhall/Sorting/Folder/Name/Component/package.dhall

let always = ../../../dhall/Predicate/always.dhall

in  F.when always [ F.originalName ]
