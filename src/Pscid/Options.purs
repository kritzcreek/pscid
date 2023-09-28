module Pscid.Options where

import Prelude

import Control.Alt ((<|>))
import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, optional)
import Data.String as String
import Effect (Effect)
import Effect.Console as Console
import Node.Platform (Platform(..))
import Node.Process (platform)
import Node.Process as Process
import Options.Applicative as OA
import Pscid.Util ((∘))

type PscidSettings a =
  { port :: a
  , buildCommand :: CLICommand
  , outputDirectory :: String
  , testCommand :: CLICommand
  , testAfterRebuild :: Boolean
  , sourceDirectories :: Array String
  , censorCodes :: Array String
  }

type PscidOptions = PscidSettings (Maybe Int)

defaultOptions :: PscidOptions
defaultOptions =
  { port: Nothing
  , buildCommand: BuildCommand (spagoCmd <> " build") []
  , outputDirectory: "output"
  , testCommand: BuildCommand (spagoCmd <> " test") []
  , testAfterRebuild: false
  , sourceDirectories: []
  , censorCodes: []
  }

-- | Scans the default directories and returns those, that did contain
-- | PureScript files.
scanDefaultDirectories :: Effect (Array String)
scanDefaultDirectories =
  let
    defaultDirectories = [ "src", "app", "test", "tests" ]
    mkGlob dir = dir <> "/**/*.purs"
  in
    Array.filterA (map (not ∘ Array.null) ∘ glob ∘ mkGlob) defaultDirectories

spagoCmd :: String
spagoCmd = if platform == Just Win32 then "spago.cmd" else "spago"

npmCmd :: String
npmCmd = if platform == Just Win32 then "npm.cmd" else "npm"

mkDefaultOptions :: Effect PscidOptions
mkDefaultOptions =
  (defaultOptions { buildCommand = _, testCommand = _, sourceDirectories = _ })
    <$> mkCommand "build"
    <*> mkCommand "test"
    <*> scanDefaultDirectories

type IncludePath = String

data CLICommand
  = ScriptCommand String
  | BuildCommand String (Array IncludePath)

printCLICommand :: CLICommand -> String
printCLICommand = case _ of
  ScriptCommand str ->
    str
  BuildCommand str [] ->
    str
  BuildCommand str includes ->
    str <> " -I " <> String.joinWith ":" includes

-- | If the command is a BuildCommand (eg. "spago build"), then the array
-- | of include paths is set. If the command is an NPM script, the command is
-- | left unchanged. This is because it's impossible to guarantee that the NPM
-- | script directly executes "spago build" (it may execute another
-- | script), and therefore we cannot simply append the includes onto the end of
-- | the command.
setCommandIncludes :: Array IncludePath -> CLICommand -> CLICommand
setCommandIncludes includesArr cmd = case cmd of
  BuildCommand str _ -> BuildCommand str includesArr
  ScriptCommand str -> ScriptCommand str

mkCommand :: String -> Effect CLICommand
mkCommand cmd = do
  pscidSpecific <- hasNamedScript ("pscid:" <> cmd)
  namedScript <- hasNamedScript cmd

  let
    npmSpecificCommand =
      guard pscidSpecific $> ScriptCommand (npmCmd <> " run -s pscid:" <> cmd)

    npmBuildCommand =
      guard namedScript $> ScriptCommand (npmCmd <> " run -s " <> cmd)

    buildCommand = BuildCommand (spagoCmd <> " " <> cmd) []

  pure $ fromMaybe buildCommand (npmSpecificCommand <|> npmBuildCommand)

-- | Accepts defaults options and
buildOptions
  :: PscidOptions
  -> { port :: Maybe Int
     , testAfterRebuild :: Boolean
     , includes :: String
     , outputDirectory :: String
     , censor :: String
     }
  -> PscidOptions
buildOptions defaults { port, testAfterRebuild, includes, outputDirectory, censor } = do
  { port
  , testAfterRebuild
  , sourceDirectories: defaults.sourceDirectories <> includesArr
  , censorCodes: sepArguments "," censor
  , outputDirectory
  , buildCommand: setCommandIncludes includesArr defaults.buildCommand
  , testCommand: setCommandIncludes includesArr defaults.testCommand
  }
  where
  includesArr = sepArguments ";" includes

  sepArguments :: String -> String -> Array String
  sepArguments sep =
    Array.filter (not String.null) ∘ String.split (String.Pattern sep)

-- | A parser for pscid's options. A `Nothing` signals the user
-- | requested the version
options :: PscidOptions -> OA.Parser (Maybe PscidOptions)
options defaults = ado
  displayVersion <- OA.switch
    ( OA.long "version"
        <> OA.help "Displays the version of this program"
    )
  port <- optional
    ( OA.option
        OA.int
        ( OA.long "port"
            <> OA.short 'p'
            <> OA.metavar "PORT"
            <> OA.help "What port to start the ide server on"
        )
    )
  testAfterRebuild <- OA.switch
    ( OA.long "test"
        <> OA.help "Run tests after successful rebuild"
    )
  includes <-
    OA.strOption
      ( OA.long "include"
          <> OA.short 'I'
          <> OA.help "Directories for additional PureScript source files, separated by `;`"
          <> OA.value ""
          <> OA.metavar "INCLUDES"
      )
      <|> pure ""
  censor <-
    OA.strOption
      ( OA.long "censor-codes"
          <> OA.help "Warning codes to ignore, seperated by `,`"
          <> OA.value ""
          <> OA.metavar "CENSOR-CODES"
      )
      <|> pure ""
  outputDirectory <-
    OA.strOption
      ( OA.long "output"
          <> OA.short 'O'
          <> OA.help "Output directory for compiled JavaScript"
          <> OA.value "output"
          <> OA.metavar "OUTPUT"
      )
      <|> pure "output"
  in
    if displayVersion then Nothing
    else Just (buildOptions defaults { port, testAfterRebuild, outputDirectory, includes, censor })

optionParser :: Effect PscidOptions
optionParser = do
  defaults <- mkDefaultOptions
  OA.execParser (opts defaults) >>= case _ of
    Nothing -> do
      Console.log =<< version
      Process.exit 0
    Just os ->
      pure os
  where
  opts defaults = OA.info (options defaults OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "Watches and rebuilds PureScript source files"
        <> OA.header "pscid - A lightweight, fast and unintrusive PureScript file-watcher"
    )

foreign import hasNamedScript :: String -> Effect Boolean
foreign import glob :: String -> Effect (Array String)
foreign import version :: Effect String
