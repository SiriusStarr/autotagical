let F = ../../../dhall/Sorting/Folder/package.dhall

let P = ../../../dhall/Predicate/package.dhall

in  F.leafFromText P.always "test"
