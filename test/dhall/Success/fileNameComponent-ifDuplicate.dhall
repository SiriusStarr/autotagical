let F = ../../../dhall/RenamingSchema/Name/Component/package.dhall

in  F.ifDuplicate [ F.duplicateNumber, F.originalName ]
