{-
# Input.TaggedValueFormat.tagsBeforeValues

## Function

Given an optional opening separator, a mandatory intermediate separator, an
optional closing separator, and a boolean indicating whether pure tags (i.e.
non-tagged values) are allowed, returns a tagged value format that recognizes
tagged values in that format (with tags coming before values).

For the absence of a separator, use `Input.Separator.none`.

## Usage

The following will match tagged values for the form `{tag value`:

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Input.TaggedValueFormat.tagsBeforeValues
      (Some (Autotagical.Input.Separator.keyword "{"))
      Autotagical.Input.Separator.whitespace
      Autotagical.Input.Separator.none
      False
```

-}

let Separator =
        ../../Internal/InputFormat/Separator sha256:0e5d8464af2df9c7ffae603f39803634d3dcecb28de50af26875820a1074c36d
      ? ../../Internal/InputFormat/Separator

let TaggedValueFormat =
        ../../Internal/InputFormat/TaggedValueFormat sha256:62b80103bf88b5ca5f35005b578267a5679fdbdcfb0b77229c3651b882961551
      ? ../../Internal/InputFormat/TaggedValueFormat

let TagValueOrder =
        ../../Internal/InputFormat/TagValueOrder sha256:2e396d0f33e0b61b28fc6526ad284de13a74cec6aaa6d3006cd3a6c24765a560
      ? ../../Internal/InputFormat/TagValueOrder

let tagsBeforeValues
    : Optional Separator →
      Separator →
      Optional Separator →
      Bool →
        TaggedValueFormat
    = λ(preTagSeparator : Optional Separator) →
      λ(tagValueSeparator : Separator) →
      λ(postValueSeparator : Optional Separator) →
      λ(pureTagsAllowed : Bool) →
        { order = TagValueOrder.TagBeforeValue
        , preTagValueSeparator = preTagSeparator
        , tagValueSeparator
        , postTagValueSeparator = postValueSeparator
        , pureTagsAllowed
        }

in  tagsBeforeValues
