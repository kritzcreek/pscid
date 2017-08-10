module Pscid.Options where

import Prelude
import Control.Monad.Eff.Console as Console
import Data.Array as Array
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (catchException)
import Data.Array (filter, filterA)
import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), split, null)
import Global (readInt)
import Node.FS (FS)
import Node.Platform (Platform(..))
import Node.Process (platform)
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (example, usage, defaultHelp, defaultVersion)
import Pscid.Util ((∘))

newtype PscidSettings a = PscidSettings
  { port              ∷ a
  , buildCommand      ∷ String
  , testCommand       ∷ String
  , testAfterRebuild  ∷ Boolean
  , sourceDirectories ∷ Array String
  , censorCodes       ∷ Array String
  }

derive instance newtypePscidSetting ∷ Newtype (PscidSettings a) _

type PscidOptions = PscidSettings (Maybe Int)

defaultOptions ∷ PscidOptions
defaultOptions = PscidSettings
  { port: Nothing
  , buildCommand: pulpCmd <> " build"
  , testCommand: pulpCmd <> " test"
  , testAfterRebuild: false
  , sourceDirectories: []
  , censorCodes: []
  }

-- | Scans the default directories and returns those, that did contain
-- | PureScript files.
scanDefaultDirectories ∷ ∀ e. Eff (fs ∷ FS | e) (Array String)
scanDefaultDirectories =
  let
    defaultDirectories = ["src", "app", "test", "tests"]
    mkGlob dir = dir <> "/**/*.purs"
  in
   filterA (map (not ∘ Array.null) ∘ glob ∘ mkGlob) defaultDirectories

pulpCmd ∷ String
pulpCmd = if platform == Just Win32 then "pulp.cmd" else "pulp"

npmCmd ∷ String
npmCmd = if platform == Just Win32 then "npm.cmd" else "npm"

mkDefaultOptions ∷ ∀ e. Eff (fs ∷ FS | e) PscidOptions
mkDefaultOptions =
  wrap <$> collectData (unwrap defaultOptions)
  where
    collectData x =
      x {buildCommand = _, testCommand = _, sourceDirectories = _}
      <$> mkCommand "build"
      <*> mkCommand "test"
      <*> scanDefaultDirectories

mkCommand ∷ ∀ e. String → Eff (fs ∷ FS | e) String
mkCommand cmd =
  hasNamedScript cmd <#> \b →
    (if b then npmCmd <> " run -s " else pulpCmd <> " ") <> cmd

optionParser ∷ ∀ e. Eff (console ∷ Console.CONSOLE, fs ∷ FS | e) PscidOptions
optionParser =
  let
    setup = usage "$0 -p 4245"
            <> example "$0 -p 4245" "Watching ... on port 4245"
            <> defaultHelp
            <> defaultVersion
  in
   catchException (const do
                      Console.error "Failed parsing the arguments."
                      Console.error "Falling back to default options"
                      mkDefaultOptions) $
     runY setup $ buildOptions
       <$> yarg "p" ["port"] (Just "The Port") (Left "") false
       <*> flag "test" [] (Just "Test project after save")
       <*> yarg "I" ["include"]
         (Just "Directories for PureScript source files, separated by `;`")
         (Left "")
         false
       <*> yarg "censor-codes" []
         (Just "Warning codes to ignore, seperated by `,`")
         (Left "")
         false

buildOptions
  ∷ ∀ e
  . String
  → Boolean
  → String
  → String
  → Eff (fs ∷ FS | e) PscidOptions
buildOptions port testAfterRebuild includes censor = do
  defaults ← unwrap <$> mkDefaultOptions
  let sourceDirectories =
        if null includes
        then defaults.sourceDirectories
        else filter (not null) (split (Pattern ";") includes)
      censorCodes = filter (not null) (split (Pattern ",") censor)
  pure (wrap { port: fromNumber (readInt 10 port)
             , testAfterRebuild
             , sourceDirectories
             , censorCodes
             , buildCommand: defaults.buildCommand
             , testCommand: defaults.testCommand
             })

foreign import hasNamedScript ∷ ∀ e. String → Eff (fs ∷ FS | e) Boolean
foreign import glob ∷ ∀ e. String → Eff (fs ∷ FS | e) (Array String)
