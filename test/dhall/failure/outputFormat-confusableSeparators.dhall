let O = ../../../dhall/Output/package.dhall

in    O.format " " "  " "   "
    ⫽ O.With.taggedValues (O.TaggedValueFormat.`tag=value` True)
