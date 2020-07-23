let I = ../../../dhall/Input/package.dhall

in  I.TaggedValueFormat.valuesBeforeTags
      (Some (I.Separator.keyword "start"))
      I.Separator.whitespace
      (Some (I.Separator.keyword ","))
      False
