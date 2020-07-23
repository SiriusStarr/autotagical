let Config =
        ./Config/package.dhall sha256:24112516be8c479893aa318bdab00185bbc7b39fc9599db208289fd6429d2964
      ? ./Config/package.dhall

in    Config.default
    ∧ { Config
      , Glob =
            ./Glob/package.dhall sha256:d9cea79579678810f84fb27db6b8794dc9b758caaa740a33cce9b9d461bd8632
          ? ./Glob/package.dhall
      , Input =
            ./Input/package.dhall sha256:4e2d9d9d75e925e8cb315cf40916e8c57b86b2eba52d7a474ba03b959f2bbacb
          ? ./Input/package.dhall
      , Output =
            ./Output/package.dhall sha256:046c8fca2323a1337a7ee62d07ee54a50e1c454dbbd28fa0ed39bd7926f4434d
          ? ./Output/package.dhall
      , Predicate =
            ./Predicate/package.dhall sha256:2b93a271f7409d961486dde301ef44b3f0e2d549b256e3f254eb6eba5863af70
          ? ./Predicate/package.dhall
      , Renaming =
            ./Renaming/package.dhall sha256:efe60f030019d8b8a661d1f7e92a043fe08bb7ffc8f9476c37195f6985638c37
          ? ./Renaming/package.dhall
      , Sorting =
            ./Sorting/package.dhall sha256:fc0ae7ee7cad2f95b6e7d686a7727385e87b8c3fd96cc3586a3d162aad0790a8
          ? ./Sorting/package.dhall
      , Tags =
            ./Tags/package.dhall sha256:0d4aa67dcbe6b530636fd134152e2c870adfcc04b9964fdf229d50998deac077
          ? ./Tags/package.dhall
      }
