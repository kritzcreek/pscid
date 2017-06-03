module Pscid.Error where

import Prelude
import Control.Monad.Eff.Console as Console
import Node.Process as Process
import Ansi.Codes (Color(Red))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (catchException, EXCEPTION)
import Pscid.Console (logColored)

catchLog
  ∷ ∀ e
  . String
  → Eff (console ∷ CONSOLE, exception ∷ EXCEPTION | e) Unit
  → Eff (console ∷ CONSOLE | e) Unit
catchLog m = catchException (const (Console.error m))

noSourceDirectoryError
  ∷ ∀ e
  . Eff (console ∷ CONSOLE, process ∷ Process.PROCESS | e) Unit
noSourceDirectoryError = do
  logColored Red "ERROR:"
  log helpString
  Process.exit 1
  where
    helpString =
      """I couldn't find any source directories to watch when trying app/, src/, test/ and tests/.
You can specify your own semicolon separated list of folders to check with the -I option like so:

$ pscid -I "sources;tests" """
