{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "pscid"
, dependencies =
  [ "aff"
  , "ansi"
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "maybe"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-path"
  , "node-process"
  , "node-streams"
  , "optparse"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psa-utils"
  , "psc-ide"
  , "refs"
  , "strings"
  , "suggest"
  , "transformers"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
