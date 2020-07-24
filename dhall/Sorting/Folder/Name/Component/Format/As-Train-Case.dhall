{-|
# Sorting.Folder.Name.Component.Format.As-Train-Case

## Component

Given a list of name components, format them as "train case", which consists
of capitalized words separated by hyphens, e.g. `As-Train-Case`.

Individual components are considered word boundaries, as well as whitespace,
case-sensitivity, and non-letter characters.  Leading and trailing whitespace is
trimmed.

**Important:** If Sorting.Folder.Name.With.separator is used, the separator will
**not** be intercalated between the components in the list passed to this
function.  (Separators will still be inserted where applicable in the components
themselves, as well as *around* this entire component.)

## Example

`["This_is-Train", "casing"]` will be converted to `"This-Is-Train-Casing"`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Sorting.Folder.Name.Component.Format.As-Train-Case
      [Autotagical.Sorting.Folder.Name.Component.text "toTrain-case"]
```

-}

let Case =
        ../../../../../Internal/NameTemplate/Case sha256:9e2d5a3c338b63a4e067de578f9e65952648ed6ffcf4ee0caaa211cdb2b3a3c5
      ? ../../../../../Internal/NameTemplate/Case

let Component =
        ../../../../../Internal/NameTemplate/Folder/Component sha256:ed3197d74431e1d211f013f10b0cfcabf17c13084847cfdd7fc519cc551fa2bb
      ? ../../../../../Internal/NameTemplate/Folder/Component

let format =
        ../../../../../Internal/NameTemplate/Folder/format sha256:c3dc42ed2f9201b36996defddd3202ef8728906c92587065f9d19dc6606047a9
      ? ../../../../../Internal/NameTemplate/Folder/format

in  format Case.AsTrainCase : List Component → Component
