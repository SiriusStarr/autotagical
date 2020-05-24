let I = ../../../dhall/OutputFormat/package.dhall

in    I.outputFormat "open" " " "close"
    ⫽ I.With.taggedValues
        ( I.TaggedValueFormat.tagsBeforeValues
            (None Text)
            "    "
            (None Text)
            True
        )
