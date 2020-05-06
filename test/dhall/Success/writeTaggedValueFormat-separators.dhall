let O = ../../../dhall/OutputFormat/package.dhall

in  O.TaggedValueFormat.valuesBeforeTags (Some "{") " " (Some "}") False
