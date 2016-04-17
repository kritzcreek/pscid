module Main where

import Prelude
import Control.Monad.Aff (launchAff, liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (head)
import Data.Either (either)
import Data.Either.Unsafe (fromRight)
import Data.Function.Eff (runEffFn2, EffFn2)
import Data.Maybe (maybe, Maybe(Nothing, Just))
import Node.ChildProcess (ChildProcess, CHILD_PROCESS)
import Node.FS (FS)
import PscIde (load, rebuild, cwd, NET)
import PscIde.Command (RebuildResult(RebuildResult), RebuildError(RebuildError), Message(Message))
import Pscid.Server (ServerStartResult(..), startServer)

port ∷ Int
port = 4243

main ∷ ∀ e. Eff ( err ∷ EXCEPTION, cp ∷ CHILD_PROCESS , console ∷ CONSOLE , net ∷ NET , avar ∷ AVAR, fs ∷ FS | e) Unit
main = launchAff do
  mCp ← serverRunning <$> startServer "psc-ide-server" 4243
  load port [] []
  Message directory ← fromRight <$> cwd port
  liftEff' $ runEffFn2 gaze (directory <> "/src/**/*.purs") (rebuildStuff port)
  liftEff $ clearConsole
  log "Watching O.O"
  pure unit

rebuildStuff
  ∷ ∀ e
    . Int
    → String
    → Eff ( net :: NET , console :: CONSOLE , err :: EXCEPTION | e) Unit
rebuildStuff p file = launchAff do
  errs ← fromRight <$> rebuild p file
  liftEff clearConsole
  log (file <>
       "\n≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡=\n" <>
       (either prettyError prettyWarning errs))

prettyError :: RebuildResult -> String
prettyError rr = "ERROR:\n" <> prettyRebuildResult rr

prettyWarning :: RebuildResult -> String
prettyWarning (RebuildResult []) = "YOU ARE AWESOME!"
prettyWarning rr = "WARNING:\n" <> prettyRebuildResult rr

prettyRebuildResult ∷ RebuildResult -> String
prettyRebuildResult (RebuildResult errs) = maybe "" pr (head errs)
  where pr (RebuildError e) = e.message

serverRunning ∷ ServerStartResult → Maybe ChildProcess
serverRunning (Started cp) = Just cp
serverRunning Closed = Nothing
serverRunning (StartError e) = Nothing

foreign import gaze ∷ ∀ eff. EffFn2 (fs ∷ FS | eff) String (String → Eff eff Unit) Unit
foreign import clearConsole ∷ ∀ e. Eff (console ∷ CONSOLE | e) Unit
-- foreign import gaze ∷ ∀ eff. String → (∀ e. String → Eff e Unit)Eff (fs ∷ FS | eff) String (String → Eff eff Unit) Unit
