module Pscid.Server
       ( restartServer
       , module PscIde.Server
       ) where

import Prelude
import Node.Process as Process
import Control.Monad.Aff (attempt, Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Nothing))
import Node.ChildProcess (CHILD_PROCESS)
import PscIde (NET)
import PscIde.Server (startServer, stopServer)

restartServer
  ∷ forall e
    . Int
    → Aff ( cp ∷ CHILD_PROCESS , net ∷ NET
          , console ∷ CONSOLE , avar ∷ AVAR
          , process ∷ Process.PROCESS | e) Unit
restartServer port = do
  attempt (stopServer port)
  r ← attempt (startServer "psc-ide-server" port Nothing)
  liftEff case r of
    Left e → do
      log ("Failed to restart psc-ide-server on port: " <> show port
           <> "\nThe error was: " <> show e)
      Process.exit 1
    Right _ → log "I restarted psc-ide-server for you."
