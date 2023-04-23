let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230408/packages.dhall
        sha256:eafb4e5bcbc2de6172e9457f321764567b33bc7279bd6952468d0d422aa33948

let overrides =
      { suggest =
        { dependencies =
          [ "console"
          , "effect"
          , "prelude"
          , "either"
          , "test-unit"
          , "node-fs"
          , "psa-utils"
          , "lists"
          , "arrays"
          , "foldable-traversable"
          , "maybe"
          , "node-buffer"
          , "ordered-collections"
          , "refs"
          , "strings"
          ]
        , repo = "https://github.com/semio/purescript-suggest.git"
        , version = "master"
        }
      }

in  upstream // overrides
