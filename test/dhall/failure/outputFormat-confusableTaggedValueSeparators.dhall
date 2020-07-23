let O = ../../../dhall/Output/package.dhall

in    O.format "open" " " "close"
    ⫽ O.With.taggedValues
        ( O.TaggedValueFormat.tagsBeforeValues
            (None Text)
            "    "
            (None Text)
            True
        )
