module Main where

import Prelude
import Node.Process as Process
import Control.Apply ((*>))
import Control.Bind ((=<<))
import Control.Monad.Aff (runAff, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (catchException, EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Monad.ST (readSTRef, modifySTRef, newSTRef, runST)
import Data.Argonaut (Json)
import Data.Array (uncons)
import Data.Either (Either, either)
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
import Pscid.Options (optionParser)
import Pscid.Psa (psaPrinter)
import Pscid.Server (startServer)

infixr 9 compose as ∘

main ∷ ∀ e. Eff ( err ∷ EXCEPTION, cp ∷ CHILD_PROCESS
                , console ∷ CONSOLE , net ∷ NET
                , avar ∷ AVAR, fs ∷ FS, process ∷ Process.PROCESS | e) Unit
main = launchAff do
  {port, buildCommand, testCommand, buildAfterSave, testAfterSave} ← liftEff optionParser
  _ ← startServer "psc-ide-server" port
  load port [] []
  Message directory ← fromRight <$> cwd port
  liftEff do
    (runEffFn2 gaze (directory <> "/src/**/*.purs") (triggerRebuild port))
    clearConsole
    initializeKeypresses
    (onKeypress (keyHandler buildCommand))
    log ("Watching " <> directory <> " on port " <> show port)
    log owl
    log "Press b to build (tries \"npm run build\" then \"pulp build\")"
    log "Press q to quit"

owl :: String
owl =
  """
  ___     ,_,        ___        ,_,     ___
 (o,o)   (o,o)   ,,,(o,o),,,   (o,o)   (o,o)
 {`"'}   {`"'}    ';:`-':;'    {`"'}   {`"'}
 -"-"-   -"-"-                 -"-"-   -"-"-
  """

keyHandler ∷ ∀ e . String → Key → Eff ( console ∷ CONSOLE
                                      , cp ∷ CHILD_PROCESS
                                      , process ∷ Process.PROCESS
                                      , fs ∷ FS | e) Unit
keyHandler buildCommand k = case k of
  Key {ctrl: false, name: "b", meta: false, shift: false} → buildProject buildCommand
  Key {ctrl: false, name: "q", meta: false, shift: false} → Console.log "Bye!" *> Process.exit 0
  Key {ctrl, name, meta, shift}                           → Console.log name

buildProject ∷ ∀ e. String → Eff (cp ∷ CHILD_PROCESS, console ∷ CONSOLE | e) Unit
buildProject buildScript =
  catchException (const (Console.log "Build Project threw an exception")) $
    runST do
      let cmd = fromJust (uncons (split " " buildScript))
      output ← newSTRef ""
      log ("Running: \"" <> buildScript <> "\"")
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
  ∷ ∀ e . Int → String → Eff ( net ∷ NET , console ∷ CONSOLE | e) Unit
triggerRebuild p file = dropFS do
  runAff
    (const (log "We couldn't talk to the server"))
    (printRebuildResult file)
    (fromRight <$> sendCommandR p (RebuildCmd file))
  where
    dropFS ∷ ∀ eff a. Eff (fs ∷ FS | eff) a → Eff eff a
    dropFS = unsafeInterleaveEff

printRebuildResult
  ∷ ∀ e. String
    → Either Json Json
    → Eff (console ∷ CONSOLE, fs ∷ FS | e) Unit
printRebuildResult file errs =
  catchException (const (Console.error "An error inside psaPrinter")) do
    clearConsole
    Console.log ("Checking " <> file)
    either (psaPrinter owl true file) (psaPrinter owl false file) errs

foreign import gaze
  ∷ ∀ eff. EffFn2 (fs ∷ FS | eff) String (String → Eff eff Unit) Unit
foreign import clearConsole ∷ ∀ e. Eff (console ∷ CONSOLE | e) Unit
