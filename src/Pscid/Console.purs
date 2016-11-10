module Pscid.Console where

import Prelude
import Ansi.Codes (Color(Blue))
import Ansi.Output (foreground, withGraphics)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (try)
import Node.Encoding (Encoding(..))
import Node.Process (stdout)
import Node.Stream (writeString)

logColored ∷ ∀ e. Color → String → Eff (console ∷ CONSOLE | e) Unit
logColored c = withGraphics log (foreground c)

owl ∷ String
owl =
  """
  ___     ,_,        ___        ,_,     ___
 (o,o)   (o,o)   ,,,(o,o),,,   (o,o)   (o,o)
 {`"'}   {`"'}    ';:`-':;'    {`"'}   {`"'}
 -"-"-   -"-"-                 -"-"-   -"-"-
  """

helpText ∷ String
helpText =
  """
Press b to run a full build (tries "npm run build" then "pulp build")
Press t to test (tries "npm run test" then "pulp test")
Press r to reset
Press q to quit
  """

startScreen ∷ ∀ e. Eff (console ∷ CONSOLE | e) Unit
startScreen = do
  log owl
  logColored Blue helpText

suggestionHint ∷ ∀ e. Eff (console ∷ CONSOLE | e) Unit
suggestionHint =
  logColored Blue "Press s to automatically apply the suggestion."

clearConsole ∷ ∀ e. Eff (console ∷ CONSOLE | e) Unit
clearConsole = void (try (writeString stdout UTF8 "\x1b[2J\x1b[;H" (pure unit)))
