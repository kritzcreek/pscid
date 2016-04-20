module Pscid.Options where

import Prelude
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (catchException)
import Data.Either (Either(Left))
import Data.Int (floor)
import Data.Maybe (Maybe(Just))
import Global (readInt)
import Node.FS (FS)
import Node.Platform (Platform(Win32))
import Node.Process (platform)
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (example, usage)

type PscidOptions =
  { port           ∷ Int
  , buildCommand   ∷ String
  , testCommand    ∷ String
  , buildAfterSave ∷ Boolean
  , testAfterSave  ∷ Boolean
  , includes       ∷ String
  }

defaultOptions ∷ PscidOptions
defaultOptions =
  { port: 4243
  , buildCommand: pulpCmd <> " build"
  , testCommand: pulpCmd <> " test"
  , buildAfterSave: false
  , testAfterSave: false
  , includes: ""
  }

pulpCmd ∷ String
pulpCmd = if platform == Win32 then "pulp.cmd" else "pulp"

npmCmd ∷ String
npmCmd = if platform == Win32 then "npm.cmd" else "npm"

mkDefaultOptions ∷ ∀ e. Eff (fs ∷ FS | e) PscidOptions
mkDefaultOptions = do
  hbs ← hasBuildScript
  pure (defaultOptions {buildCommand = if hbs
                                       then (npmCmd <> " run -s build")
                                       else (pulpCmd <> " build")})

optionParser ∷ ∀ e. Eff (console ∷ Console.CONSOLE, fs ∷ FS | e) PscidOptions
optionParser =
  let
    setup = usage "$0 -p 4245" <> example "$0 -p 4245" "Watching ... on port 4245"
  in
   catchException (const do
                      Console.error "Failed parsing the arguments."
                      Console.error "Falling back to default options"
                      mkDefaultOptions) $
     runY setup $ (\port buildAfterSave testAfterSave includes → do
                    mkDefaultOptions <#> _ { port = floor (readInt 10 port)
                                           , buildAfterSave = buildAfterSave
                                           , testAfterSave = testAfterSave
                                           , includes = includes
                                           })
       <$> yarg "p" ["port"] (Just "The Port") (Left "4243") false
       <*> flag "--build" ["build"] (Just "Build project after save")
       <*> flag "--test" ["test"] (Just "Test project after save")
       <*> yarg "I" ["include"] (Just "Additional globs for PureScript source files, separated by `;`") (Left "") false

foreign import hasBuildScript ∷ ∀ e. Eff (fs ∷ FS | e) Boolean
