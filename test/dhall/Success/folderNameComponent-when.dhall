let F = ../../../dhall/SortingSchema/Folder/Name/Component/package.dhall

let always = ../../../dhall/Predicate/always

in  F.when always [ F.originalName ]
