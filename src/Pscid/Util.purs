module Pscid.Util ((∘), shush, both, launchAffVoid) where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Just, Nothing))

infixr 9 compose as ∘

shush ∷ ∀ a b. Either a b → Maybe b
shush e = either (const Nothing) Just e

both ∷ ∀ a b. (a → b) → Either a a → Either b b
both f e = case e of
  Right x → Right (f x)
  Left x → Left (f x)

launchAffVoid ∷ ∀ a e. Aff e a → Eff (exception ∷ EXCEPTION | e) Unit
launchAffVoid a = void (launchAff a)
