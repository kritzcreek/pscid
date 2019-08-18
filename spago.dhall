{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "pscid"
, dependencies =
    [ "arrays"
    , "console"
    , "debug"
    , "node-process"
    , "node-streams"
    , "optparse"
    , "prelude"
    , "psa-utils"
    , "psc-ide"
    , "psci-support"
    , "suggest"
    , "transformers"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
