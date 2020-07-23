let O = ../../../dhall/Output/package.dhall

in    O.format "{" "-" "}"
    ⫽ O.With.tagsBeforeFileName
    ⫽ O.With.leadingTagSeparator
    ⫽ O.With.trailingTagSeparator
    ⫽ O.With.taggedValues (O.TaggedValueFormat.`tag=value` True)
