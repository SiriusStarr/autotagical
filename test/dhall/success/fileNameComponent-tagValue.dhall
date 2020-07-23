let F = ../../../dhall/Renaming/Name/Component/package.dhall

let tag = ../../../dhall/Tags/tag.dhall

in  F.tagValue (tag "test")
