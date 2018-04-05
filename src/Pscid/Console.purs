module Pscid.Console where

import Prelude
import Ansi.Codes (Color(Blue))
import Ansi.Output (foreground, withGraphics)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

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
Press b to run a full build (tries "npm run pscid:build" then "npm run build" then "pulp build")
Press t to test (tries "npm run pscid:test" then "npm run test" then "pulp test")
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

foreign import clearConsole ∷ ∀ e. Eff (console ∷ CONSOLE | e) Unit
