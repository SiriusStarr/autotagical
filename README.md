# autotagical

![CI Badge](https://github.com/SiriusStarr/autotagical/workflows/Tests/badge.svg)

`autotagical` is a utility to automagically rename and sort tagged files (such
as those produced by [TagSpaces](https://github.com/tagspaces/tagspaces))
according to user-defined schemas.  It reads in tagged files from one of more
input directories then renames and/or sorts them to an output folder hierarchy
according to rules specified in user-provided schemas.  It is intended for use
in concert with file tagging software, e.g.
[TagSpaces](https://github.com/tagspaces/tagspaces).

`autotagical` supports Linux, OS X, and Windows (see known issues).

## Getting Started/Installation

### Installation With Stack

`autotagical` may be installed by cloning this repository and running:

```bash
stack install
```

This of course depends on the [Haskell Tool Stack](https://www.haskellstack.org).

### Installation With Nix

A Nix expression is also available in `default.nix` via
[haskell.nix](https://input-output-hk.github.io/haskell.nix/).  It is
recommended that you avoid compiling GHC and such by enabling the binary cache
for `haskell.nix` by following the
[instructions here](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/#setting-up-the-cachix-binary-cache).

`autotagical` may be installed in an imperative environment with the following:

```bash
nix-env -i -f default.nix -A autotagical.components.exes.autotagical
```

In a declarative environment, you can install `autotagical` from Github with the
following addition to your `configuration.nix`:

```nix
{ config, pkgs, ... }:
with pkgs;
let

  autotagicalSource = fetchFromGitHub {
    owner = "SiriusStarr";
    repo = "autotagical";
    rev = "COMMIT";
    sha256 = "HASH";
  };

in {
  ...
  nixpkgs.config = {
    packageOverrides = pkgs: {
      autotagical = import autotagicalSource { };
    };
  };
  ...
  environment.systemPackages = with pkgs; [
    ...
    autotagical.autotagical.components.exes.autotagical
    ...
  ];
  ...
}
```

where `COMMIT` and `HASH` are the correct ID and hash for the commit you wish to
use, e.g.

```nix
autotagicalSource = fetchFromGitHub {
  owner = "SiriusStarr";
  repo = "autotagical";
  rev = "bdc5402dab3e2d9c332855b0b99ab96c6fb6f3a0";
  sha256 = "1yjkcg6krdbykzm06s57la78yb4dfxdjmr4hx347viki4jrl2yca";
};
```

You can get the correct sha256 hash with:

```bash
nix-prefetch-git https://www.github.com/SiriusStarr/autotagical COMMIT
```

### Dhall

`autotagical` uses the [Dhall configuration language](https://dhall-lang.org) to
specify all aspects of its behavior.  No installation of any kind is required to
use Dhall with `autotagical`, as any files are simply plaintext, but it may
still prove useful to make use of Dhall as a standalone program (or via an
editor, using the Dhall language server) for writing, validating, formatting,
and typechecking configurations.  Dhall allows for type-safe, programmable
configuration files with imports, making the configuration process easy,
human-readable, and non-error-prone.

## Basic Principles for Configuring `autotagical`

`autotagical` requires exactly one argument, a Dhall expression representing a
valid configuration.  This may either be a path to a Dhall file or a complete
expression, though the former is likely to be much less cumbersome.

```bash
autotagical ./configFile.dhall
```

The entire Dhall package can be imported from the file `./dhall/package.dhall`
in this repository.  This can either be done as a local import (if the Dhall
files are on your computer) or remotely off Github by importing e.g.

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall
```

You may also of course import only certain parts of the full package (each
folder has its own `package.dhall` that can be imported).

### Record Completion

`autotagical` uses record completion to build a configuration, so default
options don't have to be explicitly set.  If you're importing the full package
(as above), you can work from that record directly.  Alternately, use
`Config.default` as the base for building a configuration, e.g.

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in Autotagical::{...}
```

and

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in Autotagical.Config.default::{...}
```

are equivalent.  *Note: Neither of the above are valid configurations, only
illustrating the point.*  See the [Mandatory Settings](#Mandatory-Settings)
section for details.

### Setting Optional Behavior/Overriding Defaults

`autotagical` uses the record combination operator (`//` or `⫽`) to set
optional settings, using the pattern of "With".  For example, if one wished to
retain a copy of files in the input folders (which is not the default behavior),
one can use:

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in Autotagical::{...} ⫽ Autotagical.Config.With.keepInputCopy
```

You can see what options are available just by looking at the contents of the
various `With` records.

## Mandatory Settings

The following fields are mandatory for a config:

```dhall
inputFolders : List Text
inputFormat : InputFormat
inputPatterns : GlobPatterns
outputFolders : List Text
sortingSchema : SortingSchema
```

### `inputFolders`

`inputFolders` must be a nonempty list of paths (relative paths are fine and
will be interpreted relative to the working directory, not the config file's
location) to folders to scan for files to be moved/renamed.

### `inputFormat`

`inputFormat` is the tag format of the input files.  If you're using TagSpaces,
it's as simple as setting it to `.Input.tagSpacesFormat`.  If you're using files
tagged in a different format, you can create a custom format using
`.Input.format`, which takes an opening `Separator`, a tag `Separator`, and a
closing `Separator` and returns an `InputFormat`.  Separators may be found in
`.Input.Separator` (very simply, they may either be whitespace or a literal
keyword [one or more characters]).

As always, optional settings are available in `.Input.With`, for setting things
like whether leading/trailing separators are required and for specifying a
format for tagged values (if you're using them).

### `inputPatterns`

`inputPatterns` specifies which files in the `inputFolders` are loaded, using
standard POSIX globbing.  To input all files in the input folder, simply use the
pattern `"*"`, and to input all files in the input folder recursively (i.e.
descending into subfolders) use the pattern `"**/*"`.

### `outputFolders`

`outputFolders` must be a nonempty list of paths (relative paths are fine and
will be interpreted relative to the working directory, not the config file's
location) to folders to output files to.  If more than one folder is specified,
a complete copy of the output files will be created in each.  In general, you
probably only need to specify one unless you're interesting in creating a
duplicate output in multiple folders.

### `sortingSchema`

`sortingSchema` specifies how to sort tagged files and, as such, is the primary
function of `autotagical`.  A `SortingSchema` can be created with
`.Sorting.schema`, which takes a `List Folder`.  A `Folder` can be created with
the various functions in `.Sorting.Folder`.  Very simply, a `Folder` consists of
a `Predicate`, a `NameTemplate`, and a possibly empty list of subfolders.  A
`Predicate` is simply an expression that evaluates to `True` or `False` for a
file's tags, and a `NameTemplate` is a list of components that evaluate to
`Text` based on a file's tags.  They may be constructed with the functions in
`.Predicate` and `.Sorting.Folder.Name`, respectively.  A file will be sorted to
the first `Folder` for which the `Predicate` evaluates to `True` (with the same
happening recursively for subfolders).

## Documentation

Documentation for all Dhall functions can be found at
[https://siriusstarr.github.io/autotagical/docs](https://siriusstarr.github.io/autotagical/docs)
(generated via [dhall-docs](https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-docs)).

## Internal Dhall Files

No intended use of `autotagical` requires the (direct) use the files in
`dhall/Internal`, and the validity of configurations cannot be guaranteed if you
use any such expressions directly.  So please don't!  If you find it necessary
for some reason, please open an issue, since it probably means we've overlooked
something.

Internal files are *not* documented and do not show up in the docs, so anything
you find in the docs is fine to use.

## Known Issues

* Windows builds are broken with GHC 8.8.3.  This is due to
[a bug](https://gitlab.haskell.org/ghc/ghc/-/issues/17926) that will be
[fixed in 8.8.4](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3373).
In the meantime, building with 8.8.2 works fine.  You can force this in Stack
with the flag `--compiler=ghc-8.8.2`.

* Duplicate file checking is case-sensitive and thus may not accurately reflect
the behavior of case-insensitive filesystems (or rather those operating systems
that emulate case-insensitive behavior, e.g. HFS+ with OS X and NTFS with
Windows).  This may be fixed in a future release.

* No checking for invalid file names exists, other than the guarantee that they
will not contain the `/` character.  This is generally enough for POSIX system,
but Windows systems have a greater number of restrictions, e.g. forbidding the
following characters: `\?%*:|"<>`  An option to check text for Windows
compatibility might be added in a future release.

* Pretty-printing could be nicer.

* Path handling could be better; we'll probably want to switch to using the Path
  module

## Tests

`autotagical` may be tested by cloning this repository and running:

```bash
stack test --fast
```

from within the root directory.

## Authors

* **SiriusStarr**

## License

This project is licensed under the GNU General Public License v3.0 - see the
[LICENSE.md](LICENSE.md) file for details.
