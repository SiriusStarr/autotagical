let group = ../../../dhall/TagGroup/group

let P = ../../../dhall/Predicate/package.dhall

let tag = ../../../dhall/Tag/tag

in  P.hasGroup (group [ tag "tag1", tag "tag2" ])
