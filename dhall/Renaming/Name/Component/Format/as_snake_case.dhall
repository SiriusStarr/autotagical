{-
# Renaming.Name.Component.Format.as\_snake\_case

## Component

Given a list of name components, format them as "snake case", which consists
of all lowercase letters with words separated by underscores, e.g.
`as_snake_case`.

Individual components are considered word boundaries, as well as whitespace,
case-sensitivity, and non-letter characters.  Leading and trailing whitespace is
trimmed.

**Important:** If Renaming.Name.With.separator is used, the separator will
**not** be intercalated between the components in the list passed to this
function.  (Separators will still be inserted where applicable in the components
themselves, as well as *around* this entire component.)

## Example

`["This_is-Snake", "casing"]` will be converted to `"this_is_snake_casing"`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Renaming.Name.Component.Format.as_snake_case
      [Autotagical.Renaming.Name.Component.text "toSnake-case"]
```

-}

let Case =
        ../../../../Internal/NameTemplate/Case sha256:9e2d5a3c338b63a4e067de578f9e65952648ed6ffcf4ee0caaa211cdb2b3a3c5
      ? ../../../../Internal/NameTemplate/Case

let Component =
        ../../../../Internal/NameTemplate/File/Component sha256:b637988aae2ec7c1ee2b6fc7059008b03da8b57b1901dda79951c2045662bdfd
      ? ../../../../Internal/NameTemplate/File/Component

let format =
        ../../../../Internal/NameTemplate/File/format sha256:9412d0b754af1c6dbb1a8e6be30bb969abd3342bd31144581cb7d308677505df
      ? ../../../../Internal/NameTemplate/File/format

in  format Case.AsSnakeCase : List Component → Component