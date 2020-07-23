{-
# Output.TaggedValueFormat.tagsBeforeValues

## Function

Given an optional opening separator, a mandatory intermediate separator, an
optional closing separator, and a boolean indicating whether pure tags (i.e.
non-tagged values) are allowed, returns a tagged value format that writes tagged
values in that format (with tags coming before values).

For the absence of a separator, use `None Text`.

## Usage

The following will write tagged values for the form `{tag value`:

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Output.TaggedValueFormat.tagsBeforeValues
      (Some "{")
      " "
      (None Text)
      False
```

-}

let TaggedValueFormat =
        ../../Internal/OutputFormat/TaggedValueFormat sha256:695155008931bf451f550cd682d0057920d1498dceac6e133774c6b200c6ba40
      ? ../../Internal/OutputFormat/TaggedValueFormat

let TagValueOrder =
        ../../Internal/InputFormat/TagValueOrder sha256:2e396d0f33e0b61b28fc6526ad284de13a74cec6aaa6d3006cd3a6c24765a560
      ? ../../Internal/InputFormat/TagValueOrder

let tagsBeforeValues
    : Optional Text → Text → Optional Text → Bool → TaggedValueFormat
    = λ(preTagSeparator : Optional Text) →
      λ(tagValueSeparator : Text) →
      λ(postValueSeparator : Optional Text) →
      λ(pureTagsAllowed : Bool) →
        { order = TagValueOrder.TagBeforeValue
        , preTagValueSeparator = preTagSeparator
        , tagValueSeparator
        , postTagValueSeparator = postValueSeparator
        , pureTagsAllowed
        }

in  tagsBeforeValues
