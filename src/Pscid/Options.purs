module Pscid.Options where

import Prelude
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (catchException)
import Data.Array (cons, filter)
import Data.Either (Either(Left))
import Data.Int (floor)
import Data.Maybe (Maybe(Just))
import Data.String (split, null)
import Global (readInt)
import Node.FS (FS)
import Node.Platform (Platform(Win32))
import Node.Process (platform)
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (example, usage, defaultHelp, defaultVersion)

type PscidOptions =
  { port              ∷ Int
  , buildCommand      ∷ String
  , testCommand       ∷ String
  , testAfterRebuild  ∷ Boolean
  , sourceDirectories ∷ Array String
  }

defaultOptions ∷ PscidOptions
defaultOptions =
  { port: 4243
  , buildCommand: pulpCmd <> " build"
  , testCommand: pulpCmd <> " test"
  , testAfterRebuild: false
  , sourceDirectories: ["src"]
  }

pulpCmd ∷ String
pulpCmd = if platform == Win32 then "pulp.cmd" else "pulp"

npmCmd ∷ String
npmCmd = if platform == Win32 then "npm.cmd" else "npm"

mkDefaultOptions ∷ ∀ e. Eff (fs ∷ FS | e) PscidOptions
mkDefaultOptions =
  defaultOptions { buildCommand = _ , testCommand = _ }
    <$> mkCommand "build"
    <*> mkCommand "test"

mkCommand ∷ ∀ e. String → Eff (fs ∷ FS | e) String
mkCommand cmd =
  hasNamedScript cmd <#> \b →
    (if b then npmCmd <> " run -s " else pulpCmd <> " ") <> cmd

optionParser ∷ ∀ e. Eff (console ∷ Console.CONSOLE, fs ∷ FS | e) PscidOptions
optionParser =
  let
    setup = usage "$0 -p 4245" <> example "$0 -p 4245" "Watching ... on port 4245" <> defaultHelp <> defaultVersion
  in
   catchException (const do
                      Console.error "Failed parsing the arguments."
                      Console.error "Falling back to default options"
                      mkDefaultOptions) $
     runY setup $ (\port testAfterRebuild includes → do
                    let dirs = filter (not null) $ "src" `cons` split ";" includes
                    mkDefaultOptions <#> _ { port = floor (readInt 10 port)
                                           , testAfterRebuild = testAfterRebuild
                                           , sourceDirectories = dirs
                                           })
       <$> yarg "p" ["port"] (Just "The Port") (Left "4243") false
       <*> flag "--test" ["test"] (Just "Test project after save")
       <*> yarg "I" ["include"] (Just "Additional globs for PureScript source files, separated by `;`") (Left "") false

foreign import hasNamedScript ∷ ∀ e. String → Eff (fs ∷ FS | e) Boolean
