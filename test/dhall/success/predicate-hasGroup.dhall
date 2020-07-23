let group = ../../../dhall/Tags/group.dhall

let P = ../../../dhall/Predicate/package.dhall

let tag = ../../../dhall/Tags/tag.dhall

in  P.hasGroup (group [ tag "tag1", tag "tag2" ])
