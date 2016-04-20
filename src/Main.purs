module Main where

import Prelude
import Control.Monad.Eff.Console as Console
import Node.Process as Process
import Control.Apply ((*>))
import Control.Bind ((=<<))
import Control.Monad (when)
import Control.Monad.Aff (attempt, runAff, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (catchException, EXCEPTION)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (runReaderT, ReaderT)
import Control.Monad.ST (readSTRef, modifySTRef, newSTRef, runST)
import Control.Monad.Trans (lift)
import Data.Argonaut (Json)
import Data.Array (uncons)
import Data.Either (isLeft, Either(), either)
import Data.Either.Unsafe (fromRight)
import Data.Function.Eff (runEffFn2, EffFn2)
import Data.Functor (($>))
import Data.Maybe.Unsafe (fromJust)
import Data.String (split)
import Node.ChildProcess (stderr, stdout, Exit(BySignal, Normally), onExit, defaultSpawnOptions, spawn, CHILD_PROCESS)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.Stream (onDataString)
import PscIde (sendCommandR, load, cwd, NET)
import PscIde.Command (Command(RebuildCmd), Message(Message))
import Pscid.Keypress (Key(Key), onKeypress, initializeKeypresses)
import Pscid.Options (PscidOptions, optionParser)
import Pscid.Psa (psaPrinter)
import Pscid.Server (restartServer, stopServer, startServer)

infixr 9 compose as ∘

type Pscid e a = ReaderT PscidOptions (Eff e) a

main ∷ ∀ e. Eff ( err ∷ EXCEPTION, cp ∷ CHILD_PROCESS
                , console ∷ CONSOLE , net ∷ NET
                , avar ∷ AVAR, fs ∷ FS, process ∷ Process.PROCESS | e) Unit
main = launchAff do
  config@{port} ← liftEff optionParser
  liftEff (log "Starting psc-ide-server")
  r ← attempt $ startServer "psc-ide-server" port
  when (isLeft r) (restartServer port)
  load port [] []
  Message directory ← fromRight <$> cwd port
  liftEff do
    runEffFn2 gaze
      (directory <> "/src/**/*.purs")
      (\d → runReaderT (triggerRebuild d) config)
    clearConsole
    initializeKeypresses
    onKeypress (\k → runReaderT (keyHandler k) config)
    log ("Watching " <> directory <> " on port " <> show port)
    log owl
    log "Press b to build (tries \"npm run build\" then \"pulp build\")"
    log "Press r to reset"
    log "Press q to quit"

owl :: String
owl =
  """
  ___     ,_,        ___        ,_,     ___
 (o,o)   (o,o)   ,,,(o,o),,,   (o,o)   (o,o)
 {`"'}   {`"'}    ';:`-':;'    {`"'}   {`"'}
 -"-"-   -"-"-                 -"-"-   -"-"-
  """

keyHandler ∷ ∀ e. Key → Pscid ( console ∷ CONSOLE , cp ∷ CHILD_PROCESS
                              , process ∷ Process.PROCESS , net ∷ NET
                              , fs ∷ FS, avar ∷ AVAR | e) Unit
keyHandler k = do
  {port} ← ask
  case k of
    Key {ctrl: false, name: "b", meta: false, shift: false} →
      buildProject
    Key {ctrl: false, name: "r", meta: false, shift: false} →
      liftEff ∘ catchLog "Failed to restart server" $ launchAff do
        restartServer port
        load port [] []
    Key {ctrl: false, name: "q", meta: false, shift: false} →
      liftEff (log "Bye!" *> runAff exit exit (stopServer port))
    Key {ctrl, name, meta, shift} →
      liftEff (log name)
  where
    exit ∷ ∀ a eff. a → Eff (process ∷ Process.PROCESS | eff) Unit
    exit = const (Process.exit 0)

buildProject ∷ ∀ e. Pscid (cp ∷ CHILD_PROCESS, console ∷ CONSOLE | e) Unit
buildProject = do
  {buildCommand} ← ask
  liftEff ∘ catchLog "Build Project threw an exception" $
    runST do
      let cmd = fromJust (uncons (split " " buildCommand))
      output ← newSTRef ""
      log ("Running: \"" <> buildCommand <> "\"")
      cp ← spawn cmd.head cmd.tail defaultSpawnOptions

      let stout = stdout cp
          sterr = stderr cp

      onDataString stout UTF8 \s →
        modifySTRef output (_ <> s) $> unit

      onDataString sterr UTF8 \s →
        modifySTRef output (_ <> s) $> unit

      onExit cp \e → case e of
        Normally 0 → Console.log "Build successful!"
        Normally code → do
          log =<< readSTRef output
          log ("Build errored with code: " <> show code)
        BySignal _       → pure unit

triggerRebuild
  ∷ ∀ e . String → Pscid (net ∷ NET, console ∷ CONSOLE, fs ∷ FS | e) Unit
triggerRebuild file = do
  {port} ← ask
  lift ∘ catchLog "We couldn't talk to the server" $ launchAff do
    errs ← fromRight <$> sendCommandR port (RebuildCmd file)
    liftEff (printRebuildResult file errs)

printRebuildResult
  ∷ ∀ e. String
    → Either Json Json
    → Eff (console ∷ CONSOLE, fs ∷ FS | e) Unit
printRebuildResult file errs =
  catchLog "An error inside psaPrinter" do
    clearConsole
    Console.log ("Checking " <> file)
    either (psaPrinter owl true file) (psaPrinter owl false file) errs

foreign import gaze
  ∷ ∀ eff. EffFn2 (fs ∷ FS | eff) String (String → Eff (fs ∷ FS | eff) Unit) Unit
foreign import clearConsole ∷ ∀ e. Eff (console ∷ CONSOLE | e) Unit

catchLog
  ∷ ∀ e
  . String
  → Eff (console ∷ CONSOLE, err ∷ EXCEPTION | e) Unit
  → Eff (console ∷ CONSOLE | e) Unit
catchLog m = catchException (const (Console.error m))
