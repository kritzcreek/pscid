module Pscid.Server
       ( restartServer
       , startServer'
       , stopServer'
       , module PscIde.Server
       ) where

import Prelude
import Node.Process as Process
import PscIde as PscIde
import Control.Alt ((<|>))
import Control.Monad.Aff (attempt, Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (try)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..), maybe)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)
import PscIde (NET)
import PscIde.Command (Message(..))
import PscIde.Server (defaultServerArgs, startServer, stopServer, ServerStartResult(..), getSavedPort, pickFreshPort, savePort, deleteSavedPort)
import Pscid.Util ((∘))

stopServer'
  ∷ ∀ e
  . Int
  → Aff ( cp ∷ CHILD_PROCESS, process ∷ PROCESS
        , net ∷ NET, fs ∷ FS | e) Unit
stopServer' port = do
  _ ← liftEff (Process.cwd >>= try ∘ deleteSavedPort)
  stopServer port

type StartServerEff e =
  ( cp ∷ CHILD_PROCESS, process ∷ Process.PROCESS
  , console ∷ CONSOLE, avar ∷ AVAR
  , net ∷ NET, random ∷ RANDOM
  , fs ∷ FS | e)

startServer'
  ∷ ∀ e
  . Maybe Int
  → String
  → Aff (StartServerEff e) (Either String Int)
startServer' optPort outputDir = do
  dir ← liftEff Process.cwd
  port ← liftEff (getSavedPort dir)
  case optPort <|> port of
    Just p → do
      workingDir ← attempt (PscIde.cwd p)
      case workingDir of
        -- If we find an already running server with the right working
        -- directory, we just return its port.
        Right (Right (Message dir')) | dir == dir' → pure (Right p)
        -- Otherwise we start a new server
        _ → launchServer dir
    Nothing → launchServer dir

  where
  launchServer ∷ String → Aff (StartServerEff e) (Either String Int)
  launchServer dir = do
    newPort ← maybe (liftEff pickFreshPort) pure optPort
    _ ← liftEff (try (savePort newPort dir))
    r newPort <$>
      startServer
        ( defaultServerArgs
            { port = Just newPort
            , cwd = Just dir
            , outputDirectory = Just outputDir
            }
        )
    where
      r newPort (Started _)    = Right newPort
      r _       (Closed)       = Left "Closed"
      r _       (StartError s) = Left s

restartServer
  ∷ ∀ e
  . Int
  → String
  → Aff (StartServerEff e) Unit
restartServer port outputDir = do
  _ ← attempt (stopServer port)
  r ← attempt (startServer' (Just port) outputDir)
  liftEff case r of
    Left e → do
      log ("Failed to restart psc-ide-server on port: " <> show port
           <> "\nThe error was: " <> show e)
      Process.exit 1
    Right _ → log "I restarted psc-ide-server for you."
