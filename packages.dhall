let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230408/packages.dhall sha256:eafb4e5bcbc2de6172e9457f321764567b33bc7279bd6952468d0d422aa33948

in  upstream
  with suggest =
    { dependencies =
      [ "argonaut-codecs"
      , "argonaut-core"
      , "arrays"
      , "bifunctors"
      , "console"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "node-buffer"
      , "node-fs"
      , "node-process"
      , "node-streams"
      , "ordered-collections"
      , "prelude"
      , "psa-utils"
      , "refs"
      , "strings"
      , "test-unit"
      ]
    , repo = "https://github.com/nwolverson/purescript-suggest.git"
    , version = "c866dd7408902313c45bb579715f479f7f268162"
    }
