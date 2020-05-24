let I = ../../../dhall/InputFormat/package.dhall

in    I.inputFormat
        (I.Separator.keyword "open")
        I.Separator.whitespace
        (I.Separator.keyword "close")
    ⫽ I.With.taggedValues
        ( I.TaggedValueFormat.tagsBeforeValues
            I.Separator.none
            I.Separator.whitespace
            I.Separator.none
            True
        )
