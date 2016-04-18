module Main where

import Prelude
import Control.Monad.Eff.Console as Console
import Node.Process as Process
import Control.Apply ((*>))
import Control.Monad.Aff (runAff, launchAff, liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (catchException, EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Data.Argonaut (Json)
import Data.Array (uncons)
import Data.Either (Either(Left), either)
import Data.Either.Unsafe (fromRight)
import Data.Function.Eff (runEffFn2, EffFn2)
import Data.Int (floor)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.String (split)
import Global (readInt)
import Node.ChildProcess (Exit(Normally), onExit, defaultSpawnOptions, spawn, ChildProcess, CHILD_PROCESS)
import Node.FS (FS)
import Node.Yargs.Applicative (runY, yarg)
import Node.Yargs.Setup (example, usage)
import PscIde (sendCommandR, load, cwd, NET)
import PscIde.Command (Command(RebuildCmd), Message(Message))
import Pscid.Keypress (Key(Key), onKeypress, initializeKeypresses)
import Pscid.Psa (psaPrinter)
import Pscid.Server (ServerStartResult(..), startServer)

type PscidOptions =
  { port :: Int
  }

defaultOptions :: PscidOptions
defaultOptions = {port: 4243}

optionParser :: forall e. Eff (console :: Console.CONSOLE | e) PscidOptions
optionParser =
  let
    setup = usage "$0 -p 4245"
            <> example "$0 -p 4245" "Watching ... on port 4245"
  in
   catchException (\_ -> do
                      Console.error "Failed parsing the arguments."
                      Console.error "Falling back to default options"
                      pure defaultOptions) $
     runY setup $ (\port -> pure {port: floor (readInt 10 port)})
       <$> yarg "p" ["port"] (Just "The Port") (Left "4243") false

main ∷ ∀ e. Eff ( err ∷ EXCEPTION, cp ∷ CHILD_PROCESS
                , console ∷ Console.CONSOLE , net ∷ NET
                , avar ∷ AVAR, fs ∷ FS, process ∷ Process.PROCESS | e) Unit
main = launchAff do
  {port} <- liftEff optionParser
  mCp ← serverRunning <$> startServer "psc-ide-server" port
  load port [] []
  Message directory ← fromRight <$> cwd port
  liftEff' (runEffFn2 gaze (directory <> "/src/**/*.purs") (rebuildStuff port))
  liftEff clearConsole
  liftEff initializeKeypresses
  liftEff (onKeypress keyHandler)
  log ("Watching " <> directory <> " on port " <> show port)
  log owl

  log "Press b to build (tries \"npm run build\" then \"pulp build\")"
  log "Press q to quit"
  pure unit

owl :: String
owl =
  """
  ___     ,_,        ___        ,_,     ___
 (o,o)   (o,o)   ,,,(o,o),,,   (o,o)   (o,o)
 {`"'}   {`"'}    ';:`-':;'    {`"'}   {`"'}
 -"-"-   -"-"-                 -"-"-   -"-"-
  """

keyHandler ∷ ∀ e . Key → Eff ( console ∷ Console.CONSOLE
                             , cp ∷ CHILD_PROCESS
                             , process ∷ Process.PROCESS
                             , fs ∷ FS | e) Unit
keyHandler k = case k of
  Key {ctrl: false, name: "b", meta: false, shift: false} → getBuildScript >>= buildProject
  Key {ctrl: false, name: "q", meta: false, shift: false} → Console.log "Bye!" *> Process.exit 0
  Key {ctrl, name, meta, shift} → Console.log name

buildProject ∷ ∀ e. Maybe String → Eff (cp ∷ CHILD_PROCESS, console ∷ Console.CONSOLE | e) Unit
buildProject buildScript = do
  cp <- fromMaybe
    (spawn "pulp" ["build"] defaultSpawnOptions) $
    (\{head, tail} → spawn head tail defaultSpawnOptions) <$> (buildScript >>= uncons <<< split " ")
  onExit cp \e → case e of
    (Normally errcode) → Console.log ("Build exited with status: " <> show errcode)
    _ → Console.log "Build terminated by Signal."

rebuildStuff
  ∷ ∀ e . Int → String → Eff ( net ∷ NET , console ∷ Console.CONSOLE | e) Unit
rebuildStuff p file = dropFS do
  runAff
    (const (Console.log "We couldn't talk to the server"))
    (printRebuildResult file)
    (fromRight <$> sendCommandR p (RebuildCmd file))
  where
    dropFS ∷ ∀ eff a. Eff (fs ∷ FS | eff) a → Eff eff a
    dropFS = unsafeInterleaveEff

printRebuildResult
  ∷ ∀ e. String
    → Either Json Json
    → Eff (console ∷ Console.CONSOLE, fs ∷ FS | e) Unit
printRebuildResult file errs =
  catchException (const (Console.error "An error inside psaPrinter")) do
    clearConsole
    Console.log ("Checking " <> file)
    either (psaPrinter owl true file) (psaPrinter owl false file) errs

serverRunning ∷ ServerStartResult → Maybe ChildProcess
serverRunning (Started cp) = Just cp
serverRunning Closed = Nothing
serverRunning (StartError e) = Nothing

foreign import gaze
  ∷ ∀ eff. EffFn2 (fs ∷ FS | eff) String (String → Eff eff Unit) Unit
foreign import clearConsole ∷ ∀ e. Eff (console ∷ Console.CONSOLE | e) Unit

foreign import getBuildScriptImpl ∷ ∀ e. EffFn2 (fs ∷ FS | e) (Maybe String) (String → Maybe String) (Maybe String)

getBuildScript ∷ ∀ e. Eff (fs ∷ FS | e) (Maybe String)
getBuildScript = runEffFn2 getBuildScriptImpl Nothing Just

