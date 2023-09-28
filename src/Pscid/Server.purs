module Pscid.Server
  ( restartServer
  , startServer'
  , stopServer'
  , module PscIde.Server
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (try)
import Node.ChildProcess as CP
import Node.Process as Process
import PscIde as PscIde
import PscIde.Command (Message(..))
import PscIde.Server (ServerStartResult(..), defaultServerArgs, deleteSavedPort, getSavedPort, pickFreshPort, savePort, startServer, stopServer)
import Pscid.Util ((∘))

stopServer' :: Int -> Aff Unit
stopServer' port = do
  _ <- liftEffect (Process.cwd >>= try ∘ deleteSavedPort)
  stopServer port

startServer'
  :: Maybe Int
  -> String
  -> Aff (Either String Int)
startServer' optPort outputDir = do
  dir <- liftEffect Process.cwd
  port <- liftEffect (getSavedPort dir)
  case optPort <|> port of
    Just p -> do
      workingDir <- attempt (PscIde.cwd p)
      case workingDir of
        -- If we find an already running server with the right working
        -- directory, we just return its port.
        Right (Right (Message dir')) | dir == dir' -> pure (Right p)
        -- Otherwise we start a new server
        _ -> launchServer dir
    Nothing -> launchServer dir

  where
  launchServer :: String -> Aff (Either String Int)
  launchServer dir = do
    newPort <- maybe (liftEffect pickFreshPort) pure optPort
    _ <- liftEffect (try (savePort newPort dir))
    r newPort <$>
      startServer
        ( defaultServerArgs
            { port = Just newPort
            , cwd = Just dir
            , outputDirectory = Just outputDir
            , stdio = CP.ignore
            }
        )
    where
    r newPort (Started _) = Right newPort
    r _ (Closed) = Left "Closed"
    r _ (StartError s) = Left s

restartServer :: Int -> String -> Aff Unit
restartServer port outputDir = do
  _ <- attempt (stopServer port)
  r <- attempt (startServer' (Just port) outputDir)
  liftEffect case r of
    Left e -> do
      Console.log
        ( "Failed to restart psc-ide-server on port: " <> show port
            <> "\nThe error was: "
            <> show e
        )
      Process.exit 1
    Right _ -> Console.log "I restarted psc-ide-server for you."
