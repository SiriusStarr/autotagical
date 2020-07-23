let I = ../../../dhall/Input/package.dhall

in    I.format
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
