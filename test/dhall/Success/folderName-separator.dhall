let F = ../../../dhall/SortingSchema/Folder/Name/package.dhall

in    F.template [ F.Component.text "test", F.Component.originalName ]
    ⫽ F.With.separator ","
