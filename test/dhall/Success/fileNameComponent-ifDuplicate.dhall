let F = ../../../dhall/Renaming/Name/Component/package.dhall

in  F.ifDuplicate [ F.duplicateNumber, F.originalName ]
