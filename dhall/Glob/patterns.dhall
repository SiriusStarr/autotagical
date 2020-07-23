{-
# Glob.patterns

## Function

Compile a list of `Text` patterns into glob patterns, used for input, ignore,
and unnamed file matching.  These patterns follow standard POSIX glob behavior;
for more details, check out the documentation of
[System.FilePath.Glob](http://hackage.haskell.org/package/Glob-0.10.0/docs/System-FilePath-Glob.html#v:compile),
which is what is used internally.

## Default Behavior

By default, error recovery is turned off.  Patterns that contain syntactical
errors will fail to compile, resulting in an error when you try to import the
config into `autotagical`.  To override this behavior, use
`Glob.With.errorRecovery`.

**Important:** All glob patterns are **relative** to the input folder.  This
means that, if a file is in a subdirectory of the input folder, `*.jpg` will not
match it.  Use `**` if you need to match path separators as well, e.g. to match
files in subfolders as well.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Glob.patterns [ "*.jpg", "*.png" ]
```

-}

let GlobPatterns =
        ../Internal/GlobPatterns/Type sha256:26a29e0113646fb623fba2a6657b31b99127b689d510ef6761df7dd49da8a5bb
      ? ../Internal/GlobPatterns/Type

let patterns
    : List Text → GlobPatterns
    = λ(patterns : List Text) → { errorRecovery = False, patterns }

in  patterns
