module Pscid.Keypress where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Prelude

newtype Key =
  Key
  { ctrl ∷ Boolean
  , name ∷ String
  , meta ∷ Boolean
  , shift ∷ Boolean
  }

foreign import initializeKeypresses ∷ ∀ e. Eff (console ∷ CONSOLE | e) Unit
foreign import onKeypress ∷ ∀ e. (Key → Eff (console ∷ CONSOLE | e) Unit) → Eff (console ∷ CONSOLE | e) Unit

