module Pscid.Util ((∘), shush) where

import Data.Either (Either, either)
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (const, compose)

infixr 9 compose as ∘

shush ∷ ∀ a b. Either a b → Maybe b
shush e = either (const Nothing) Just e
