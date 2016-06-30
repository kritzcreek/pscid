module Pscid.Server
       ( restartServer
       , startServer'
       , stopServer'
       , module PscIde.Server
       ) where

import Prelude
import Node.Process as Process
import Control.Monad.Aff (attempt, Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Alt ( (<|>) )
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..), maybe)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)
import PscIde (NET)
import PscIde.Command (Message(..))
import PscIde.Server (startServer, stopServer, ServerStartResult(..), getSavedPort, pickFreshPort, savePort, deleteSavedPort)

stopServer' :: forall eff. Int -> Aff (cp :: CHILD_PROCESS, process :: PROCESS, net :: NET, fs :: FS, err :: EXCEPTION | eff) Unit
stopServer' port = do
  dir <- liftEff Process.cwd
  liftEff (deleteSavedPort dir)
  stopServer port

startServer' ∷ forall eff. Maybe Int
  → Aff (cp ∷ CHILD_PROCESS, process :: Process.PROCESS, console ∷ CONSOLE, avar ∷ AVAR, net :: NET, random :: RANDOM, fs :: FS, err :: EXCEPTION | eff) (Either String Int)
startServer' optPort = do
  dir <- liftEff Process.cwd
  port <- liftEff (getSavedPort dir)
  case optPort <|> port of
    Just p -> do
      workingDir <- attempt (PscIde.cwd p)
      case workingDir of
        Right (Right (Message dir')) | dir == dir' -> pure (Right p)
        _ -> launchServer dir
    Nothing -> launchServer dir

  where
  launchServer :: String -> Aff (cp ∷ CHILD_PROCESS, process :: Process.PROCESS, console ∷ CONSOLE, avar ∷ AVAR, net :: NET, random :: RANDOM, fs :: FS, err :: EXCEPTION | eff) (Either String Int)
  launchServer dir = do
    newPort <- maybe (liftEff pickFreshPort) pure optPort
    liftEff (savePort newPort dir)
    r newPort <$> startServer "psc-ide-server" newPort (Just dir)
    where
      r newPort (Started _)    = Right newPort
      r _       (Closed)       = Left "Closed"
      r _       (StartError s) = Left s

restartServer
  ∷ forall e
    . Int
    → Aff ( cp ∷ CHILD_PROCESS , net ∷ NET
          , console ∷ CONSOLE , avar ∷ AVAR
          , process ∷ Process.PROCESS | e) Unit
restartServer port = do
  dir <- liftEff Process.cwd
  attempt (stopServer port)
  r ← attempt (startServer "psc-ide-server" port Nothing)
  liftEff case r of
    Left e → do
      log ("Failed to restart psc-ide-server on port: " <> show port
           <> "\nThe error was: " <> show e)
      Process.exit 1
    Right _ → log "I restarted psc-ide-server for you."
