module Main where

import Prelude
import Control.Monad.Eff.Console as Console
import Control.Monad.Aff (runAff, launchAff, liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (catchException, EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Data.Argonaut (Json)
import Data.Either (Either, either)
import Data.Either.Unsafe (fromRight)
import Data.Function.Eff (runEffFn2, EffFn2)
import Data.Maybe (Maybe(Nothing, Just))
import Node.ChildProcess (ChildProcess, CHILD_PROCESS)
import Node.FS (FS)
import PscIde (sendCommandR, load, cwd, NET)
import PscIde.Command (Command(RebuildCmd), Message(Message))
import Pscid.Psa (psaPrinter)
import Pscid.Server (ServerStartResult(..), startServer)

port ∷ Int
port = 4243

main ∷ ∀ e. Eff ( err ∷ EXCEPTION, cp ∷ CHILD_PROCESS
                , console ∷ Console.CONSOLE , net ∷ NET
                , avar ∷ AVAR, fs ∷ FS | e) Unit
main = launchAff do
  mCp ← serverRunning <$> startServer "psc-ide-server" 4243
  load port [] []
  Message directory ← fromRight <$> cwd port
  liftEff' (runEffFn2 gaze (directory <> "/src/**/*.purs") (rebuildStuff port))
  liftEff $ clearConsole
  log "Watching \\O/"
  pure unit

rebuildStuff
  ∷ ∀ e
    . Int
    → String
    → Eff ( net ∷ NET
          , console ∷ Console.CONSOLE | e) Unit
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
printRebuildResult file errs = catchException (const (Console.error "An error inside psaPrinter")) do
  clearConsole
  Console.log ("Checking " <> file)
  either (psaPrinter true file) (psaPrinter false file) errs

serverRunning ∷ ServerStartResult → Maybe ChildProcess
serverRunning (Started cp) = Just cp
serverRunning Closed = Nothing
serverRunning (StartError e) = Nothing

foreign import gaze ∷ ∀ eff. EffFn2 (fs ∷ FS | eff) String (String → Eff eff Unit) Unit
foreign import clearConsole ∷ ∀ e. Eff (console ∷ Console.CONSOLE | e) Unit
