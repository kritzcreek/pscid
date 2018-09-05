module Pscid.Options where

import Prelude

import Control.Alt ((<|>))
import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.Newtype as Newtype
import Data.String as String
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (catchException)
import Global (readInt)
import Node.Platform (Platform(..))
import Node.Process (platform)
import Node.Yargs.Applicative (flag, runY, yarg)
import Node.Yargs.Setup (defaultHelp, defaultVersion, example, usage)
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
scanDefaultDirectories ∷ Effect (Array String)
scanDefaultDirectories =
  let
    defaultDirectories = ["src", "app", "test", "tests"]
    mkGlob dir = dir <> "/**/*.purs"
  in
   Array.filterA (map (not ∘ Array.null) ∘ glob ∘ mkGlob) defaultDirectories

pulpCmd ∷ String
pulpCmd = if platform == Just Win32 then "pulp.cmd" else "pulp"

npmCmd ∷ String
npmCmd = if platform == Just Win32 then "npm.cmd" else "npm"

mkDefaultOptions ∷ Effect PscidOptions
mkDefaultOptions =
  Newtype.traverse PscidSettings collectData defaultOptions
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
      str <> " -I " <> (String.joinWith ":" includesArr)

-- | If the command is a PulpCommand (eg. "pulp build"), then the array of
-- | include paths is set. If the command is an NPM script, the command is
-- | left unchanged. This is because it's impossible to guarantee that the NPM
-- | script directly executes "pulp build" (it may execute another script), and
-- | therefore we cannot simply append the includes onto the end of the command.
setCommandIncludes :: Array IncludePath -> CLICommand -> CLICommand
setCommandIncludes includesArr cmd = case cmd of
  PulpCommand str _ -> PulpCommand str includesArr
  ScriptCommand str -> ScriptCommand str

mkCommand ∷ String → Effect CLICommand
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

optionParser ∷ Effect PscidOptions
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
  ∷ String
  → Boolean
  → String
  → String
  → String
  → Effect PscidOptions
buildOptions port testAfterRebuild includes outputDirectory censor = do
  defaults ← un PscidSettings <$> mkDefaultOptions
  let includesArr = Array.filter (not String.null) (String.split (String.Pattern ";") includes)
      sourceDirectories = defaults.sourceDirectories <> includesArr
      censorCodes = Array.filter (not String.null) (String.split (String.Pattern ",") censor)
      buildCommand = setCommandIncludes includesArr defaults.buildCommand
      testCommand = setCommandIncludes includesArr defaults.testCommand

  pure (PscidSettings
    { port: Int.fromNumber (readInt 10 port)
    , testAfterRebuild
    , sourceDirectories
    , censorCodes
    , buildCommand
    , outputDirectory
    , testCommand
    })

foreign import hasNamedScript ∷ String → Effect Boolean
foreign import glob ∷ String → Effect (Array String)
