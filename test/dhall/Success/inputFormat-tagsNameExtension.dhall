let I = ../../../dhall/InputFormat/package.dhall

in    I.inputFormat
        I.Separator.whitespace
        (I.Separator.keyword ",")
        I.Separator.whitespace
    ⫽ I.With.tagsBeforeFileName
    ⫽ I.With.taggedValues (I.TaggedValueFormat.`tag=value` True)
