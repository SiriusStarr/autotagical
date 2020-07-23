let F = ../../../dhall/Renaming/Name/package.dhall

in    F.template [ F.Component.text "text", F.Component.originalName ]
    ⫽ F.With.separator "a / in sep"
