{ clobberDestination =
      ./clobberDestination.dhall sha256:b6e3d069effc85fbf49b9e81cec8db55c4896b7e9032f920f9b991aae0aa2c66
    ? ./clobberDestination.dhall
, dryRun =
      ./dryRun.dhall sha256:f315c5497db797933dc8591919e51d0b44bead38a87d4aad39006c92dbb2d4c7
    ? ./dryRun.dhall
, ignorePatterns =
      ./ignorePatterns.dhall sha256:a088d5df5be35cd12afa08021010992475cb7287035158d6c5e7bbaa7d0defcb
    ? ./ignorePatterns.dhall
, keepInputCopy =
      ./keepInputCopy.dhall sha256:4b472bee5a7dd511f5167b05dcfc8ac1feee83519f358fb7c23cc734e19fa58a
    ? ./keepInputCopy.dhall
, LogLevel =
      ./LogLevel/package.dhall sha256:0a8db2fca7baf776d41672f00c5b8ad8e58b0ab922d2fe5f3b13023d55b8ce34
    ? ./LogLevel/package.dhall
, LogTo =
      ./LogTo/package.dhall sha256:e0b8be744b92ddb5e0370d6d218580218e39c72abe05a1e58a2f9a7f096f6fc1
    ? ./LogTo/package.dhall
, newOutputFormat =
      ./newOutputFormat.dhall sha256:9da4e4fd9b80bc4ea341b54ada243e3c15a0beb7745f7fa5437460a852e73cad
    ? ./newOutputFormat.dhall
, renaming =
      ./renaming.dhall sha256:cdb5200a280883fdc6407fbb85be2081897337b8a987e07840586b643543b7a4
    ? ./renaming.dhall
}
