let O = ../../../dhall/OutputFormat/package.dhall

in    O.outputFormat "{" "-" "}"
    ⫽ O.With.tagsBeforeFileName
    ⫽ O.With.leadingTagSeparator
    ⫽ O.With.trailingTagSeparator
    ⫽ O.With.taggedValues (O.TaggedValueFormat.`tag=value` True)
