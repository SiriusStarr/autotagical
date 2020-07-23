let I = ../../../dhall/Input/package.dhall

in    I.format
        I.Separator.whitespace
        (I.Separator.keyword ",")
        I.Separator.whitespace
    ⫽ I.With.tagsBeforeFileName
    ⫽ I.With.taggedValues (I.TaggedValueFormat.`tag=value` True)
