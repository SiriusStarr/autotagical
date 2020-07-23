let I = ../../../dhall/Input/package.dhall

in  I.TaggedValueFormat.tagsBeforeValues
      I.Separator.none
      (I.Separator.keyword "a/b")
      I.Separator.none
      False
