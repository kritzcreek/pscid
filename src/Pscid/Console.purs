module Pscid.Console where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Effect (Effect)
import Effect.Console as Console
import Pscid.Util ((∘))

logColored ∷ Color → String → Effect Unit
logColored c = Console.log ∘ withGraphics (foreground c)

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
Press b to run a full build (tries "npm run pscid:build" then "npm run build" then "spago build")
Press t to test (tries "npm run pscid:test" then "npm run test" then "spago test")
Press r to reset
Press q to quit
  """

startScreen ∷ Effect Unit
startScreen = do
  Console.log owl
  logColored Blue helpText

suggestionHint ∷ Effect Unit
suggestionHint =
  logColored Blue "Press s to automatically apply the suggestion."

foreign import clearConsole ∷ Effect Unit
