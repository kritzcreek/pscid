module Pscid.Util ((∘), both) where

import Prelude

import Data.Either (Either(..))

infixr 9 compose as ∘

both :: forall a b. (a -> b) -> Either a a -> Either b b
both f e = case e of
  Right x -> Right (f x)
  Left x -> Left (f x)
