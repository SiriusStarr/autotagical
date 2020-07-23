let I = ../../../dhall/Input/package.dhall

in    I.format
        I.Separator.whitespace
        I.Separator.whitespace
        I.Separator.whitespace
    ⫽ I.With.taggedValues (I.TaggedValueFormat.`tag=value` True)
