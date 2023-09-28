module Pscid.Util (both) where

import Data.Either (Either(..))

both :: forall a b. (a -> b) -> Either a a -> Either b b
both f e = case e of
  Right x -> Right (f x)
  Left x -> Left (f x)
