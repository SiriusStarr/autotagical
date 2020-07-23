{-
# Input.With.tagsBeforeFileName

## Default Behavior

By default, input formats assume that files are tagged in the order
`File Name -> Tags -> File Extension`.

## Option Behavior

This option sets the input format to expect files tagged in the order
`Tags -> File Name -> Extension`.

**Important:** Reversing the order impacts greediness with respect to extension
parsing.  Specifically, the parser prefers extensions to file names.  For
example, the file: `[tag1,tag2,tag3]file.name.tar.gz` will parse with the file
name: `file` and the extensions `.name.tar.gz`.  It *will* always parse a
nonempty file name, however, so dotfiles will parse correctly, e.g.
`[tag1,tag2,tag3].file.name.tar.gz` will parse with the file name: `.file` and
the extensions `.name.tar.gz`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in    Autotagical.Input.tagSpacesFormat
    ⫽ Autotagical.Input.With.tagsBeforeFileName
```

-}

let Order =
        ../../Internal/InputFormat/Order sha256:77ea7cca209ab0ef9afd003a580efa8aba29cf572cbe095698679fb25a96d24c
      ? ../../Internal/InputFormat/Order

in  { tagOrder = Order.TagsNameExtension } : { tagOrder : Order }
