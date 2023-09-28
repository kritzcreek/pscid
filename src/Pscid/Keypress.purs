module Pscid.Keypress where

import Prelude

import Effect (Effect)

newtype Key = Key
  { ctrl :: Boolean
  , name :: String
  , meta :: Boolean
  , shift :: Boolean
  }

foreign import initializeKeypresses :: Effect Unit
foreign import onKeypress :: (Key -> Effect Unit) -> Effect Unit
