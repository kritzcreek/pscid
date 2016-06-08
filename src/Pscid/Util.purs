module Pscid.Util ((∘), shush, both) where

import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (const, compose)

infixr 9 compose as ∘

shush ∷ ∀ a b. Either a b → Maybe b
shush e = either (const Nothing) Just e

both ∷ ∀ a b. (a → b) → Either a a → Either b b
both f e = case e of
  Right x → Right (f x)
  Left x → Left (f x)
