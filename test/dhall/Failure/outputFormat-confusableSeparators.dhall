let I = ../../../dhall/OutputFormat/package.dhall

in    I.outputFormat " " "  " "   "
    ⫽ I.With.taggedValues (I.TaggedValueFormat.`tag=value` True)
