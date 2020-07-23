let O = ../../../dhall/Output/package.dhall

in  O.TaggedValueFormat.valuesBeforeTags (Some "{") " " (Some "}") False
