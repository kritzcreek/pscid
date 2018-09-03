module Pscid.Options where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (catchException)
import Control.MonadZero (guard)
import Data.Array (filter, filterA)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), joinWith, null, split)
import Global (readInt)
import Node.FS (FS)
import Node.Platform (Platform(..))
import Node.Process (platform)
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (example, usage, defaultHelp, defaultVersion)
import Pscid.Util ((∘))

newtype PscidSettings a = PscidSettings
  { port              ∷ a
  , buildCommand      ∷ CLICommand
  , outputDirectory   ∷ String
  , testCommand       ∷ CLICommand
  , testAfterRebuild  ∷ Boolean
  , sourceDirectories ∷ Array String
  , censorCodes       ∷ Array String
  }

derive instance newtypePscidSetting ∷ Newtype (PscidSettings a) _

type PscidOptions = PscidSettings (Maybe Int)

defaultOptions ∷ PscidOptions
defaultOptions = PscidSettings
  { port: Nothing
  , buildCommand: PulpCommand (pulpCmd <> " build") []
  , outputDirectory: "output"
  , testCommand: PulpCommand (pulpCmd <> " test") []
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

type IncludePath = String

data CLICommand
  = ScriptCommand String
  | PulpCommand String (Array IncludePath)

printCLICommand :: CLICommand -> String
printCLICommand = case _ of
  ScriptCommand str -> str
  PulpCommand str includesArr ->
    if Array.null includesArr then
      str
    else
      str <> " -I " <> (joinWith ":" includesArr)

-- | If the command is a PulpCommand (eg. "pulp build"), then the array of
-- | include paths is set. If the command is an NPM script, the command is
-- | left unchanged. This is because it's impossible to guarantee that the NPM
-- | script directly executes "pulp build" (it may execute another script), and
-- | therefore we cannot simply append the includes onto the end of the command.
setCommandIncludes :: Array IncludePath -> CLICommand -> CLICommand
setCommandIncludes includesArr cmd = case cmd of
  PulpCommand str _ -> PulpCommand str includesArr
  ScriptCommand str -> ScriptCommand str

mkCommand ∷ ∀ e. String → Eff (fs ∷ FS | e) CLICommand
mkCommand cmd = do
  pscidSpecific ← hasNamedScript ("pscid:" <> cmd)
  namedScript   ← hasNamedScript cmd

  let specificCommand =
        guard pscidSpecific $> ScriptCommand ("npm run -s pscid:" <> cmd)

      buildCommand =
        guard namedScript $> ScriptCommand ("npm run -s " <> cmd)

      pulpCommand =
        PulpCommand (pulpCmd <> " " <> cmd) []

  pure $ fromMaybe pulpCommand (specificCommand <|> buildCommand)

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
       <*> yarg "O" ["output"]
         (Just "Output directory for compiled JavaScript")
         (Left "output")
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
  → String
  → Eff (fs ∷ FS | e) PscidOptions
buildOptions port testAfterRebuild includes outputDirectory censor = do
  defaults ← unwrap <$> mkDefaultOptions
  let includesArr = filter (not null) (split (Pattern ";") includes)
      sourceDirectories = defaults.sourceDirectories <> includesArr
      censorCodes = filter (not null) (split (Pattern ",") censor)
      buildCommand = setCommandIncludes includesArr defaults.buildCommand
      testCommand = setCommandIncludes includesArr defaults.testCommand

  pure (wrap { port: fromNumber (readInt 10 port)
             , testAfterRebuild
             , sourceDirectories
             , censorCodes
             , buildCommand
             , outputDirectory
             , testCommand
             })

foreign import hasNamedScript ∷ ∀ e. String → Eff (fs ∷ FS | e) Boolean
foreign import glob ∷ ∀ e. String → Eff (fs ∷ FS | e) (Array String)
