{-|
# Config.default

## Record Completion

`autotagical` uses record completion to build a configuration, so default
options don't have to be explicitly set.  If you're importing the full package,
you can work from that record directly instead of using this file.

The following fields are mandatory:

```dhall
{ inputFolders : List Text
, inputFormat : InputFormat
, inputPatterns : GlobPatterns
, outputFolders : List Text
, sortingSchema : SortingSchema
}
```

## Multiply Input Files

In general, it is bad practice to use input folders that overlap, e.g. using
`[ "./folder", "./folder/subfolder"]` if any patterns match recursively.  More
specifically, situations in which files are matched via multiple input folders
can result in unanticipated behavior.

In the case that a file is input via multiple input folders, the information of
the *last* is used.  However, **matches that determine the file to be unnamed
are preferred to those that find it to be named** (if `Config.With.renaming` is
used).

Additionally, if **any match finds that the file should be ignored** (if
`Config.With.ignorePatterns` is used), it will be, overriding any other inputs.

## Default Behavior

By default, `autotagical` will:

* Not overwrite destination files.
* Actually move files.
* Not ignore any input files.
* Not keep a copy of files (files will be moved, not copied).
* Log only warnings and errors.
* Log to `stdout`.
* Not rewrite tags in a new format.
* Not rename unnamed files.

To override any of this behavior, use `Config.With`.

## Usage

The following are equivalent:

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical::{
    , inputFolders = [ "input" ]
    , inputFormat = Autotagical.Input.tagSpacesFormat
    , inputPatterns = Autotagical.Glob.patterns [ "*.jpg", "*.png" ]
    , outputFolders = [ "output" ]
    , sortingSchema =
        Autotagical.Sorting.schema
          [ Autotagical.Sorting.Folder.leaf
              "folder"
              Autotagical.Predicate.always
          ]
    }
```

and

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Config.default::{
    , inputFolders = [ "input" ]
    , inputFormat = Autotagical.Input.tagSpacesFormat
    , inputPatterns = Autotagical.Glob.patterns [ "*.jpg", "*.png" ]
    , outputFolders = [ "output" ]
    , sortingSchema =
        Autotagical.Sorting.schema
          [ Autotagical.Sorting.Folder.leaf
              "folder"
              Autotagical.Predicate.always
          ]
    }
```

-}

let Config =
        ../Internal/Config/Type sha256:7ef0deccb653c8ae143d2c42e625b2d748123b4e4b1bdc835c8c706fb71e2d39
      ? ../Internal/Config/Type

let GlobPatterns =
        ../Internal/GlobPatterns/Type sha256:26a29e0113646fb623fba2a6657b31b99127b689d510ef6761df7dd49da8a5bb
      ? ../Internal/GlobPatterns/Type

let LogDestination =
        ../Internal/Logging/LogDestination sha256:47393d4c5bdc46697670d9b6d4c73c4875c1df2f2cf2f1cbe68b2afe03c0816f
      ? ../Internal/Logging/LogDestination

let LogLevel =
        ../Internal/Logging/LogLevel sha256:8b0ef97f0f052a52dd67e11fa031d0efe3ff18c020f9911ab2179066dee3922e
      ? ../Internal/Logging/LogLevel

let OutputFormat =
        ../Internal/OutputFormat/Type sha256:90a967648257956b041755ef770b4d5a1adce53345a9443d872080c2f511ca2c
      ? ../Internal/OutputFormat/Type

let Renaming =
        ../Internal/Config/Renaming sha256:7023726a4e39329b1e662bff1e4ef99e230db28751a5097ff541b406d93f2f4e
      ? ../Internal/Config/Renaming

let DefaultOptions =
      { clobberDestination : Optional Text
      , dryRun : Bool
      , ignorePatterns : Optional GlobPatterns
      , keepCopyInInputFolder : Bool
      , logDestination : LogDestination
      , logLevel : LogLevel
      , outputFormat : Optional OutputFormat
      , renaming : Optional Renaming
      }

in    { Type = Config
      , default =
        { clobberDestination = None Text
        , dryRun = False
        , ignorePatterns = None GlobPatterns
        , keepCopyInInputFolder = False
        , logDestination = LogDestination.StdOut
        , logLevel = LogLevel.Warn
        , outputFormat = None OutputFormat
        , renaming = None Renaming
        }
      }
    : { Type : Type, default : DefaultOptions }
