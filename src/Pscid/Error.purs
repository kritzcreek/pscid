module Pscid.Error where

import Prelude

import Ansi.Codes (Color(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (catchException)
import Node.Process as Process
import Pscid.Console (logColored)

catchLog :: String -> Effect Unit -> Effect Unit
catchLog m = catchException (const (Console.error m))

noSourceDirectoryError :: Effect Unit
noSourceDirectoryError = do
  logColored Red "ERROR:"
  Console.log helpString
  Process.exit 1
  where
  helpString =
    """I couldn't find any source directories to watch when trying app/, src/, test/ and tests/.
You can specify your own semicolon separated list of folders to check with the -I option like so:

$ pscid -I "sources;tests" """
