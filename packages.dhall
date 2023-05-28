let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230528/packages.dhall sha256:7f1ebc6968ebabae915640456faf98f52f5606189f48cb22305fbe008723f26c

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
      ]
    , repo = "https://github.com/nwolverson/purescript-suggest.git"
    , version = "c866dd7408902313c45bb579715f479f7f268162"
    }
