let I = ../../../dhall/InputFormat/package.dhall

in    I.inputFormat
        I.Separator.whitespace
        I.Separator.whitespace
        I.Separator.whitespace
    ⫽ I.With.taggedValues (I.TaggedValueFormat.`tag=value` True)
