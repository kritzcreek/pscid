module Pscid.Server where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff (Aff, later', makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Par (Par(Par), runPar)
import Control.Alt ((<|>))
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, Exit(Normally), onClose, onError, defaultSpawnOptions, spawn)
import PscIde (NET, quit)

data ServerStartResult =
  Started ChildProcess
  | Closed
  | StartError String

-- | Start a psc-ide server instance, or find one already running on the expected port, checking if it has the right path.
startServer
  :: forall eff
  . String
  -> Int
  -> Aff (cp :: CHILD_PROCESS, console :: CONSOLE, net :: NET, avar :: AVAR | eff) ServerStartResult
startServer exe port = do
    liftEff $ log "Starting psc-ide-server"
    cp <- liftEff $ spawn exe ["-p", show port] defaultSpawnOptions
    let handleErr = makeAff $ \_ succ -> do
                      onError cp (\_ -> succ $ StartError "psc-ide-server error")
                      onClose cp (\exit -> case exit of
                                     (Normally 0) -> succ Closed
                                     (Normally n) -> succ $ StartError $ "Error code returned: "++ show n
                                     _ -> succ $ StartError "Other close error")

    runPar (Par handleErr <|> Par (later' 100 $ pure $ Started cp))

-- | Stop a psc-ide server. Currently implemented by asking it nicely, but potentially by killing it if that doesn't work...
stopServer :: forall eff. Int -> Aff (cp :: CHILD_PROCESS, net :: NET | eff) Unit
stopServer port = do
  res <- quit port
  pure unit
